#' Validate classifier inputs
#'
#' @param x A data.frame, tibble, data.table, or matrix.
#' @param y A factor, character, or integer vector.
#' @return A factor vector (validated and possibly coerced).
#' @keywords internal
validate_classifier_inputs <- function(x, y) {
  if (nrow(x) != length(y)) {
    rlang::abort("x and y must have the same number of observations.")
  }
  if (is.factor(y)) {
    return(y)
  }
  if (is.character(y)) {
    return(as.factor(y))
  }
  if (is.integer(y)) {
    return(as.factor(y))
  }
  rlang::abort(
    "y must be a factor, character, or integer vector for classification.",
    class = "rtabpfn_input_error"
  )
}

#' Validate regressor inputs
#'
#' @param x A data.frame, tibble, data.table, or matrix.
#' @param y A numeric vector.
#' @return A double vector (validated).
#' @keywords internal
validate_regressor_inputs <- function(x, y) {
  if (nrow(x) != length(y)) {
    rlang::abort("x and y must have the same number of observations.")
  }
  if (!is.numeric(y)) {
    rlang::abort(
      "y must be numeric for regression.",
      class = "rtabpfn_input_error"
    )
  }
  as.double(y)
}

#' Map R model version string to Python ModelVersion enum
#'
#' @param version A string: `"v2"`, `"v2.5"`, or `"v2.6"`, or `NULL`.
#' @return A Python `ModelVersion` enum value, or `NULL` if `version` is `NULL`.
#' @keywords internal
resolve_model_version <- function(version) {
  if (is.null(version)) {
    return(NULL)
  }
  valid <- c("v2", "v2.5", "v2.6")
  if (!version %in% valid) {
    rlang::abort(
      paste0(
        "model_version must be one of: ",
        paste0('"', valid, '"', collapse = ", "),
        ". Got: \"", version, '"'
      ),
      class = "rtabpfn_input_error"
    )
  }
  mv <- tabpfn_constants$ModelVersion
  switch(version,
    "v2"   = mv$V2,
    "v2.5" = mv$V2_5,
    "v2.6" = mv$V2_6
  )
}
