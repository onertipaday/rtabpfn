#' Check if TabPFN Python package is available
#'
#' @return `TRUE` invisibly if available; otherwise aborts with a helpful
#'   error message.
#' @keywords internal
check_tabpfn_available <- function() {
  if (!reticulate::py_module_available("tabpfn")) {
    rlang::abort(
      c(
        "The TabPFN Python package is not installed.",
        i = "Run `rtabpfn::install_tabpfn()` to install it.",
        i = "Or install manually: `pip install tabpfn`"
      ),
      class = "rtabpfn_missing_python"
    )
  }
  invisible(TRUE)
}

#' Wrap a Python call with R error handling
#'
#' Catches Python exceptions and re-throws them as classed R conditions
#' with cleaned-up messages.
#'
#' @param expr An expression that calls Python via reticulate.
#' @return The result of `expr`.
#' @keywords internal
wrap_python_error <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("python|Python|tabpfn", msg, ignore.case = TRUE)) {
        clean_msg <- sub(".*Error:\\s*", "", msg)
        rlang::abort(
          paste0("TabPFN error: ", clean_msg),
          class = "rtabpfn_python_error",
          parent = e
        )
      } else {
        rlang::abort(msg, parent = e)
      }
    }
  )
}

#' Convert R tabular data to a numpy array
#'
#' Accepts data.frame, tibble, data.table, or matrix. Factors and character
#' columns are ordinal-encoded (0-indexed). Returns a float32 numpy array.
#'
#' @param x A data.frame, tibble, data.table, or matrix.
#' @return A numpy array (float32).
#' @keywords internal
ensure_numpy_array <- function(x) {
  if (inherits(x, "data.table")) {
    x <- as.data.frame(x)
  }
  if (is.data.frame(x)) {
    x <- as.data.frame(lapply(x, function(col) {
      if (is.factor(col) || is.character(col)) {
        as.numeric(as.factor(col)) - 1L
      } else {
        as.numeric(col)
      }
    }))
    reticulate::np_array(as.matrix(x), dtype = "float32")
  } else if (is.matrix(x)) {
    reticulate::np_array(x, dtype = "float32")
  } else {
    rlang::abort(
      "Expected data.frame, tibble, data.table, or matrix.",
      class = "rtabpfn_input_error"
    )
  }
}
