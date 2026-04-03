#' Fit a TabPFN Regressor
#'
#' Trains a TabPFN regressor on tabular data.
#'
#' @param x A data.frame, tibble, data.table, matrix of predictors, or a
#'   formula (e.g., `mpg ~ .`).
#' @param ... Arguments passed to methods. For the formula method, these are
#'   forwarded to the default (XY) method.
#'
#' @return An object of class `"tabpfn_regressor"`.
#'
#' @examples
#' \dontrun{
#' # XY interface
#' fit <- tabpfn_regressor(mtcars[, -1], mtcars$mpg)
#' predict(fit, mtcars[1:5, -1])
#'
#' # Formula interface
#' fit <- tabpfn_regressor(mpg ~ ., data = mtcars)
#' predict(fit, mtcars[1:5, ])
#' }
#'
#' @export
tabpfn_regressor <- function(x, ...) {
  UseMethod("tabpfn_regressor")
}

#' @rdname tabpfn_regressor
#' @param y A numeric vector of target values.
#' @param n_estimators Integer. Number of ensemble members. Default `8L`.
#' @param softmax_temperature Double. Temperature for softmax. Default `0.9`.
#' @param device Character. `"auto"`, `"cpu"`, or `"cuda"`. Default `"auto"`.
#' @param model_version Character or `NULL`. Default `NULL` (v2.6).
#' @param random_state Integer. Random seed. Default `0L`.
#' @param ignore_pretraining_limits Logical. If `TRUE`, bypass the row and
#'   column limit checks (class limit is always enforced). Default `FALSE`.
#' @param training_set_limit Integer or `Inf`. Maximum number of training
#'   samples. If the training set exceeds this limit, it is stratified-subsampled
#'   (by outcome quartile) to this size. Default `Inf` (no subsampling).
#' @param control A `control_tabpfn` object created by [control_tabpfn()], or
#'   `NULL` (default) for Python defaults.
#' @export
tabpfn_regressor.default <- function(x, y,
                                      n_estimators = 8L,
                                      softmax_temperature = 0.9,
                                      device = "auto",
                                      model_version = NULL,
                                      random_state = 0L,
                                      ignore_pretraining_limits = FALSE,
                                      training_set_limit = Inf,
                                      control = NULL,
                                      ...) {
  y <- validate_regressor_inputs(x, y)

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
  y_np <- reticulate::np_array(y, dtype = "float64")

  py_version <- resolve_model_version(model_version)
  version_label <- model_version %||% .tabpfn_default_version

  # Merge control args with explicit args
  base_args <- list(
    n_estimators = as.integer(n_estimators),
    softmax_temperature = softmax_temperature,
    device = device,
    random_state = as.integer(random_state)
  )
  ctrl_args <- if (!is.null(control)) unclass(control) else list()
  all_args <- c(base_args, ctrl_args, list(...))

  reg <- wrap_python_error({
    if (!is.null(py_version)) {
      do.call(
        tabpfn_module$TabPFNRegressor$create_default_for_version,
        c(list(py_version), all_args)
      )
    } else {
      do.call(tabpfn_module$TabPFNRegressor, all_args)
    }
  })

  py_log <- reticulate::py_capture_output(
    wrap_python_error(reg$fit(x_np, y_np))
  )

  structure(
    list(
      model_type    = "regressor",
      model_version = version_label,
      n_features    = ncol(x),
      n_train       = nrow(x),
      feature_names = feature_names,
      params        = list(
        n_estimators = n_estimators,
        softmax_temperature = softmax_temperature,
        device = device
      ),
      control = control,
      logging = py_log,
      .py = reg
    ),
    class = "tabpfn_regressor"
  )
}

#' @rdname tabpfn_regressor
#' @param data A data.frame containing both predictors and outcome (used with
#'   the formula interface).
#' @export
tabpfn_regressor.formula <- function(x, data, ...) {
  mf <- stats::model.frame(x, data = data)
  y <- stats::model.response(mf)
  predictors <- mf[, -1, drop = FALSE]
  tabpfn_regressor.default(predictors, y, ...)
}

#' Predict with a TabPFN Regressor
#'
#' @param object A fitted `tabpfn_regressor` object.
#' @param new_data A data.frame, tibble, data.table, or matrix.
#' @param type Character. `"numeric"` (mean), `"median"`, `"mode"`, or `"quantile"`.
#' @param quantiles Numeric vector required when `type = "quantile"`.
#' @param ... Not used.
#'
#' @return A tibble.
#'
#' @export
predict.tabpfn_regressor <- function(object, new_data, type = "numeric",
                                      quantiles = NULL, ...) {
  valid_types <- c("numeric", "median", "mode", "quantile")
  if (!type %in% valid_types) {
    rlang::abort(paste0(
      'type must be one of: ', paste0('"', valid_types, '"', collapse = ", "),
      '. Got: "', type, '"'
    ))
  }

  new_data <- select_training_features(new_data, object$feature_names)
  x_np <- ensure_numpy_array(new_data)
  n_rows <- nrow(new_data)

  if (type == "quantile") {
    if (is.null(quantiles)) {
      rlang::abort("quantiles must be provided when type = \"quantile\".")
    }
    q_preds <- wrap_python_error(
      object$.py$predict(x_np, output_type = "quantiles",
                         quantiles = as.list(quantiles))
    )
    # reticulate converts Python list to R list (1-indexed)
    rows <- lapply(seq_along(quantiles), function(i) {
      tibble::tibble(
        .row = seq_len(n_rows),
        .quantile = quantiles[i],
        .pred = as.double(reticulate::py_to_r(q_preds[[i]]))
      )
    })
    dplyr::bind_rows(rows)
  } else {
    py_output_type <- switch(type,
      numeric = "mean",
      median  = "median",
      mode    = "mode"
    )
    preds_raw <- wrap_python_error(
      object$.py$predict(x_np, output_type = py_output_type)
    )
    tibble::tibble(.pred = as.double(reticulate::py_to_r(preds_raw)))
  }
}

#' @export
print.tabpfn_regressor <- function(x, ...) {
  cat("# TabPFN Regressor (", x$model_version, ")\n", sep = "")
  cat("# ", x$n_features, " features, trained on ",
      x$n_train, " samples\n", sep = "")
  cat("# n_estimators=", x$params$n_estimators,
      ", device=", x$params$device, "\n", sep = "")
  invisible(x)
}

#' Augment data with TabPFN regressor predictions
#'
#' Binds numeric predictions to the input data.
#'
#' @param x A fitted `tabpfn_regressor` object.
#' @param new_data A data.frame, tibble, data.table, or matrix.
#' @param ... Not used.
#'
#' @return A tibble with original data plus a `.pred` column.
#'
#' @export
augment.tabpfn_regressor <- function(x, new_data, ...) {
  preds <- predict(x, new_data, type = "numeric")
  new_data <- tibble::as_tibble(new_data)
  dplyr::bind_cols(new_data, preds)
}

#' @export
summary.tabpfn_regressor <- function(object, ...) {
  print(object)
  cat("\nFeatures: ", paste(object$feature_names, collapse = ", "), "\n")
  cat("\nParameters:\n")
  for (nm in names(object$params)) {
    cat("  ", nm, ": ", format(object$params[[nm]]), "\n", sep = "")
  }
  invisible(object)
}
