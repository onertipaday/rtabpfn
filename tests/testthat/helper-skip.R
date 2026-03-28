skip_if_no_tabpfn <- function() {
  if (!reticulate::py_module_available("tabpfn")) {
    testthat::skip("tabpfn Python package not available")
  }
}
