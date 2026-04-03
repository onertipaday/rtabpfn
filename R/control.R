#' Control Options for TabPFN
#'
#' Creates a control object for advanced TabPFN configuration options that are
#' passed through to the Python backend.
#'
#' @param n_preprocessing_jobs Integer. Number of worker processes for
#'   preprocessing. `-1L` uses all available cores. Default `1L`.
#' @param fit_mode Character. Controls preprocessing and caching:
#'   `"fit_preprocessors"` (default), `"low_memory"`, `"fit_with_cache"`,
#'   or `"batched"`.
#' @param memory_saving_mode Character or logical. Helps with out-of-memory
#'   errors. `"auto"` (default), `TRUE`, or `FALSE`.
#' @param inference_precision Character. Trade-off between speed and
#'   reproducibility: `"auto"` (default), `"autocast"`, or a torch dtype string.
#'
#' @return A list of class `"control_tabpfn"`.
#'
#' @examples
#' control_tabpfn()
#' control_tabpfn(fit_mode = "low_memory", n_preprocessing_jobs = -1L)
#'
#' @export
control_tabpfn <- function(n_preprocessing_jobs = 1L,
                            fit_mode = "fit_preprocessors",
                            memory_saving_mode = "auto",
                            inference_precision = "auto") {
  valid_fit_modes <- c("fit_preprocessors", "low_memory", "fit_with_cache", "batched")
  if (!fit_mode %in% valid_fit_modes) {
    rlang::abort(
      paste0(
        "fit_mode must be one of: ",
        paste0('"', valid_fit_modes, '"', collapse = ", "),
        '. Got: "', fit_mode, '"'
      ),
      class = "rtabpfn_input_error"
    )
  }

  if (!is.character(inference_precision)) {
    rlang::abort("inference_precision must be a character string.",
                 class = "rtabpfn_input_error")
  }

  structure(
    list(
      n_preprocessing_jobs = as.integer(n_preprocessing_jobs),
      fit_mode = fit_mode,
      memory_saving_mode = memory_saving_mode,
      inference_precision = inference_precision
    ),
    class = "control_tabpfn"
  )
}

#' @export
print.control_tabpfn <- function(x, ...) {
  defaults <- control_tabpfn()
  cat("# TabPFN control options\n")
  for (nm in names(x)) {
    val <- x[[nm]]
    marker <- if (!identical(val, defaults[[nm]])) " *" else ""
    cat("#   ", nm, ": ", format(val), marker, "\n", sep = "")
  }
  invisible(x)
}
