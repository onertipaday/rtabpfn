#' Fit a TabPFN Classifier
#'
#' Trains a TabPFN classifier on tabular data. TabPFN is a transformer-based
#' model that performs in-context learning on the training data.
#'
#' @param x A data.frame, tibble, data.table, matrix of predictors, or a
#'   formula (e.g., `Species ~ .`).
#' @param ... Arguments passed to methods. For the formula method, these are
#'   forwarded to the default (XY) method.
#'
#' @return An object of class `"tabpfn_classifier"`.
#'
#' @examples
#' \dontrun{
#' # XY interface
#' fit <- tabpfn_classifier(iris[, 1:4], iris$Species)
#' predict(fit, iris[1:10, 1:4])
#'
#' # Formula interface
#' fit <- tabpfn_classifier(Species ~ ., data = iris)
#' predict(fit, iris[1:10, ])
#' }
#'
#' @export
tabpfn_classifier <- function(x, ...) {
  UseMethod("tabpfn_classifier")
}

#' @rdname tabpfn_classifier
#' @param y A factor, character, or integer vector of class labels.
#' @param n_estimators Integer. Number of ensemble members. Default `8L`.
#' @param softmax_temperature Double. Temperature for softmax. Default `0.9`.
#' @param balance_probabilities Logical. Balance class probabilities.
#'   Default `FALSE`.
#' @param device Character. Device for computation: `"auto"`, `"cpu"`, or
#'   `"cuda"`. Default `"auto"`.
#' @param model_version Character or `NULL`. TabPFN model version:
#'   `"v2"`, `"v2.5"`, `"v2.6"`, or `NULL` (default, uses v2.6).
#' @param random_state Integer. Random seed. Default `0L`.
#' @param ignore_pretraining_limits Logical. If `TRUE`, bypass the row and
#'   column limit checks (class limit is always enforced). Default `FALSE`.
#' @param training_set_limit Integer or `Inf`. Maximum number of training
#'   samples. If the training set exceeds this limit, it is stratified-subsampled
#'   (by class) to this size. Default `Inf` (no subsampling).
#' @param control A `control_tabpfn` object created by [control_tabpfn()], or
#'   `NULL` (default) for Python defaults.
#' @export
tabpfn_classifier.default <- function(x, y,
                                       n_estimators = 8L,
                                       softmax_temperature = 0.9,
                                       balance_probabilities = FALSE,
                                       device = "auto",
                                       model_version = NULL,
                                       random_state = 0L,
                                       ignore_pretraining_limits = FALSE,
                                       training_set_limit = Inf,
                                       control = NULL,
                                       ...) {
  y <- validate_classifier_inputs(x, y)

  # Subsample before constraint check so limit can bring data within bounds
  sub_idx <- subsample_training_set(x, y, training_set_limit)
  if (!is.null(sub_idx)) {
    x <- x[sub_idx, , drop = FALSE]
    y <- y[sub_idx]
  }

  check_data_constraints(x, y, ignore_limits = ignore_pretraining_limits)
  check_tabpfn_available()
  feature_names <- colnames(x)
  x_np <- ensure_numpy_array(x)
  y_np <- as.integer(as.numeric(y) - 1L)

  py_version <- resolve_model_version(model_version)
  version_label <- model_version %||% "v2.6"

  # Merge control args with explicit args
  base_args <- list(
    n_estimators = as.integer(n_estimators),
    softmax_temperature = softmax_temperature,
    balance_probabilities = balance_probabilities,
    device = device,
    random_state = as.integer(random_state)
  )
  ctrl_args <- if (!is.null(control)) unclass(control) else list()
  all_args <- c(base_args, ctrl_args, list(...))

  clf <- wrap_python_error({
    if (!is.null(py_version)) {
      do.call(
        tabpfn_module$TabPFNClassifier$create_default_for_version,
        c(list(py_version), all_args)
      )
    } else {
      do.call(tabpfn_module$TabPFNClassifier, all_args)
    }
  })

  py_log <- reticulate::py_capture_output(
    wrap_python_error(clf$fit(x_np, y_np))
  )

  y <- droplevels(y)
  structure(
    list(
      model_type    = "classifier",
      model_version = version_label,
      classes       = levels(y),
      n_classes     = nlevels(y),
      n_features    = ncol(x),
      n_train       = nrow(x),
      feature_names = feature_names,
      params        = list(
        n_estimators = n_estimators,
        softmax_temperature = softmax_temperature,
        balance_probabilities = balance_probabilities,
        device = device
      ),
      control = control,
      logging = py_log,
      .py = clf
    ),
    class = "tabpfn_classifier"
  )
}

#' @rdname tabpfn_classifier
#' @param data A data.frame containing both predictors and outcome (used with
#'   the formula interface).
#' @export
tabpfn_classifier.formula <- function(x, data, ...) {
  mf <- stats::model.frame(x, data = data)
  y <- stats::model.response(mf)
  predictors <- mf[, -1, drop = FALSE]
  tabpfn_classifier.default(predictors, y, ...)
}

#' Predict with a TabPFN Classifier
#'
#' @param object A fitted `tabpfn_classifier` object.
#' @param new_data A data.frame, tibble, data.table, or matrix of new observations.
#' @param type Character. Type of prediction: `"class"` for predicted labels,
#'   `"prob"` for class probabilities. Default `"class"`.
#' @param ... Not used.
#'
#' @return A tibble. For `type = "class"`, a column `.pred_class` (factor).
#'   For `type = "prob"`, columns `.pred_{classname}` (double).
#'
#' @export
predict.tabpfn_classifier <- function(object, new_data, type = "class", ...) {
  valid_types <- c("class", "prob")
  if (!type %in% valid_types) {
    rlang::abort(paste0(
      'type must be one of: ', paste0('"', valid_types, '"', collapse = ", "),
      '. Got: "', type, '"'
    ))
  }

  # If new_data has extra columns (e.g., the outcome from formula interface),
  # select only the feature columns used during training
  if (is.data.frame(new_data) && !is.null(object$feature_names)) {
    matching <- intersect(object$feature_names, colnames(new_data))
    if (length(matching) == length(object$feature_names)) {
      new_data <- new_data[, object$feature_names, drop = FALSE]
    }
  }

  x_np <- ensure_numpy_array(new_data)

  if (type == "class") {
    preds_raw <- wrap_python_error(object$.py$predict(x_np))
    preds_int <- as.integer(reticulate::py_to_r(preds_raw)) + 1L
    pred_labels <- object$classes[preds_int]
    tibble::tibble(.pred_class = factor(pred_labels, levels = object$classes))
  } else {
    probs_raw <- wrap_python_error(object$.py$predict_proba(x_np))
    probs <- reticulate::py_to_r(probs_raw)
    colnames(probs) <- paste0(".pred_", object$classes)
    tibble::as_tibble(probs)
  }
}

#' @export
print.tabpfn_classifier <- function(x, ...) {
  cat("# TabPFN Classifier (", x$model_version, ")\n", sep = "")
  cat("# ", x$n_classes, " classes, ",
      x$n_features, " features, trained on ",
      x$n_train, " samples\n", sep = "")
  cat("# n_estimators=", x$params$n_estimators,
      ", device=", x$params$device, "\n", sep = "")
  invisible(x)
}

#' @importFrom generics augment
#' @export
generics::augment

#' Augment data with TabPFN classifier predictions
#'
#' Binds class predictions and probabilities to the input data.
#'
#' @param x A fitted `tabpfn_classifier` object.
#' @param new_data A data.frame, tibble, data.table, or matrix.
#' @param ... Not used.
#'
#' @return A tibble with original data plus `.pred_class` and probability columns.
#'
#' @export
augment.tabpfn_classifier <- function(x, new_data, ...) {
  preds_class <- predict(x, new_data, type = "class")
  preds_prob  <- predict(x, new_data, type = "prob")
  new_data <- tibble::as_tibble(new_data)
  dplyr::bind_cols(new_data, preds_class, preds_prob)
}

#' @export
summary.tabpfn_classifier <- function(object, ...) {
  print(object)
  cat("\nClasses: ", paste(object$classes, collapse = ", "), "\n")
  cat("Features: ", paste(object$feature_names, collapse = ", "), "\n")
  cat("\nParameters:\n")
  for (nm in names(object$params)) {
    cat("  ", nm, ": ", format(object$params[[nm]]), "\n", sep = "")
  }
  invisible(object)
}
