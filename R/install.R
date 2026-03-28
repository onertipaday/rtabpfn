#' Install the TabPFN Python package
#'
#' Installs the TabPFN Python package into a dedicated virtual environment
#' or conda environment. This is a convenience wrapper around
#' [reticulate::py_install()].
#'
#' @param method Installation method. Default `"auto"` lets reticulate choose.
#' @param conda Path to conda binary. Default `"auto"`.
#' @param pip Logical; use pip for installation. Default `TRUE`.
#' @param python_version Minimum Python version. Default `">=3.9"`.
#' @param envname Name of the virtual environment. Default `"r-tabpfn"`.
#' @param ... Additional arguments passed to [reticulate::py_install()].
#'
#' @export
install_tabpfn <- function(method = "auto", conda = "auto",
                           pip = TRUE, python_version = ">=3.9",
                           envname = "r-tabpfn", ...) {
  reticulate::py_install(
    "tabpfn",
    envname = envname,
    method = method,
    conda = conda,
    pip = pip,
    python_version = python_version,
    ...
  )
}
