#' Fit a TabPFN Classifier
#'
#' Trains a TabPFN classifier on tabular data. TabPFN is a transformer-based
#' model that performs in-context learning on the training data.
#'
#' @param x A data.frame, tibble, data.table, or matrix of predictors.
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
#' @param ... Additional arguments passed to the Python `TabPFNClassifier`.
#'
#' @return An object of class `"tabpfn_classifier"`.
#'
#' @examples
#' \dontrun{
#' fit <- tabpfn_classifier(iris[, 1:4], iris$Species)
#' predict(fit, iris[1:10, 1:4])
#' predict(fit, iris[1:10, 1:4], type = "prob")
#' }
#'
#' @export
tabpfn_classifier <- function(x, y,
                               n_estimators = 8L,
                               softmax_temperature = 0.9,
                               balance_probabilities = FALSE,
                               device = "auto",
                               model_version = NULL,
                               random_state = 0L,
                               ...) {
  y <- validate_classifier_inputs(x, y)
  check_tabpfn_available()
  feature_names <- colnames(x)
  x_np <- ensure_numpy_array(x)
  y_np <- as.integer(as.numeric(y) - 1L)

  py_version <- resolve_model_version(model_version)
  version_label <- model_version %||% "v2.6"

  clf <- wrap_python_error({
    if (!is.null(py_version)) {
      tabpfn_module$TabPFNClassifier$create_default_for_version(
        py_version,
        n_estimators = as.integer(n_estimators),
        softmax_temperature = softmax_temperature,
        balance_probabilities = balance_probabilities,
        device = device,
        random_state = as.integer(random_state),
        ...
      )
    } else {
      tabpfn_module$TabPFNClassifier(
        n_estimators = as.integer(n_estimators),
        softmax_temperature = softmax_temperature,
        balance_probabilities = balance_probabilities,
        device = device,
        random_state = as.integer(random_state),
        ...
      )
    }
  })

  wrap_python_error(clf$fit(x_np, y_np))

  structure(
    list(
      model_type    = "classifier",
      model_version = version_label,
      classes       = levels(droplevels(y)),
      n_classes     = nlevels(droplevels(y)),
      n_features    = ncol(x),
      n_train       = nrow(x),
      feature_names = feature_names,
      params        = list(
        n_estimators = n_estimators,
        softmax_temperature = softmax_temperature,
        balance_probabilities = balance_probabilities,
        device = device
      ),
      .py = clf
    ),
    class = "tabpfn_classifier"
  )
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
