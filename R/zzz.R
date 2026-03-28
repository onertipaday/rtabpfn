# Module-level reference, populated on load
tabpfn_module <- NULL
tabpfn_constants <- NULL

.onLoad <- function(libname, pkgname) {
  tabpfn_module <<- reticulate::import("tabpfn", delay_load = TRUE)
  tabpfn_constants <<- reticulate::import("tabpfn.constants", delay_load = TRUE)

  # Register parsnip engine if parsnip is installed and the registration
  # function has been defined (i.e., the parsnip module is loaded)
  if (requireNamespace("parsnip", quietly = TRUE) &&
      exists("register_tabpfn_parsnip", mode = "function")) {
    register_tabpfn_parsnip()
  }
}
