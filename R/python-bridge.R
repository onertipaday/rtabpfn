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
