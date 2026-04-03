# Module-level reference, populated on load
tabpfn_module <- NULL
tabpfn_constants <- NULL

#' Check for libomp conflict on macOS
#'
#' On macOS, loading OpenMP (libomp) before PyTorch can cause a segfault.
#' This function detects the conflict and warns the user.
#'
#' @return Invisible `NULL`. Called for side effects (warning on conflict).
#' @keywords internal
check_libomp <- function() {
  if (Sys.info()[["sysname"]] != "Darwin") {
    return(invisible(NULL))
  }
  vm_output <- tryCatch(
    system(paste("vmmap", Sys.getpid()), intern = TRUE, ignore.stderr = TRUE),
    error = function(e) character(0),
    warning = function(w) character(0)
  )
  if (length(vm_output) > 0 && any(grepl("libomp", vm_output))) {
    rlang::warn(
      c(
        "An existing package has loaded OpenMP (libomp).",
        x = "PyTorch may segfault if it tries to load its own copy.",
        i = "Try running `reticulate::import('torch')` in a fresh R session before loading other packages.",
        i = "See https://github.com/pytorch/pytorch/issues/78490 for details."
      ),
      class = "rtabpfn_libomp_warning"
    )
  }
  invisible(NULL)
}

.onLoad <- function(libname, pkgname) {
  check_libomp()

  # Declare Python dependency so reticulate can auto-install via ephemeral uv
  # environments when tabpfn is not already available.
  tryCatch(
    reticulate::py_require("tabpfn"),
    error = function(e) NULL  # non-fatal: install_tabpfn() is the fallback
  )

  tabpfn_module <<- reticulate::import("tabpfn", delay_load = TRUE)
  tabpfn_constants <<- reticulate::import("tabpfn.constants", delay_load = TRUE)

  # Register parsnip engine if parsnip is installed and the registration
  # function has been defined (i.e., the parsnip module is loaded)
  if (requireNamespace("parsnip", quietly = TRUE) &&
      exists("register_tabpfn_parsnip", mode = "function")) {
    register_tabpfn_parsnip()
  }
}
