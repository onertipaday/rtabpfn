#' Fit a TabPFN Regressor
#'
#' Trains a TabPFN regressor on tabular data.
#'
#' @param x A data.frame, tibble, data.table, or matrix of predictors.
#' @param y A numeric vector of target values.
#' @param n_estimators Integer. Number of ensemble members. Default `8L`.
#' @param softmax_temperature Double. Temperature for softmax. Default `0.9`.
#' @param device Character. `"auto"`, `"cpu"`, or `"cuda"`. Default `"auto"`.
#' @param model_version Character or `NULL`. Default `NULL` (v2.6).
#' @param random_state Integer. Random seed. Default `0L`.
#' @param ... Additional arguments passed to the Python `TabPFNRegressor`.
#'
#' @return An object of class `"tabpfn_regressor"`.
#'
#' @examples
#' \dontrun{
#' fit <- tabpfn_regressor(mtcars[, -1], mtcars$mpg)
#' predict(fit, mtcars[1:5, -1])
#' }
#'
#' @export
tabpfn_regressor <- function(x, y,
                              n_estimators = 8L,
                              softmax_temperature = 0.9,
                              device = "auto",
                              model_version = NULL,
                              random_state = 0L,
                              ...) {
  y <- validate_regressor_inputs(x, y)
  check_tabpfn_available()
  feature_names <- colnames(x)
  x_np <- ensure_numpy_array(x)
  y_np <- reticulate::np_array(y, dtype = "float64")

  py_version <- resolve_model_version(model_version)
  version_label <- model_version %||% "v2.6"

  reg <- wrap_python_error({
    if (!is.null(py_version)) {
      tabpfn_module$TabPFNRegressor$create_default_for_version(
        py_version,
        n_estimators = as.integer(n_estimators),
        softmax_temperature = softmax_temperature,
        device = device,
        random_state = as.integer(random_state),
        ...
      )
    } else {
      tabpfn_module$TabPFNRegressor(
        n_estimators = as.integer(n_estimators),
        softmax_temperature = softmax_temperature,
        device = device,
        random_state = as.integer(random_state),
        ...
      )
    }
  })

  wrap_python_error(reg$fit(x_np, y_np))

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
      .py = reg
    ),
    class = "tabpfn_regressor"
  )
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
