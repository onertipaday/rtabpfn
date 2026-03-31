#' Extract Embeddings from a Fitted TabPFN Model
#'
#' Returns the transformer's internal representations for each observation.
#'
#' @param object A fitted `tabpfn_classifier` or `tabpfn_regressor`.
#' @param new_data A data.frame, tibble, data.table, or matrix of observations.
#' @param data_source Character. `"test"` (default) or `"train"` -- which
#'   transformer tokens to return.
#' @param aggregate Character. How to aggregate across estimators:
#'   `"mean"` (default) averages, `"concat"` concatenates, `"none"` returns
#'   the raw 3-D array.
#'
#' @return A matrix `(n_samples, embedding_dim)` for `"mean"` or `"concat"`,
#'   or a 3-D array `(n_estimators, n_samples, embedding_dim)` for `"none"`.
#'
#' @examples
#' \dontrun{
#' fit <- tabpfn_classifier(iris[, 1:4], iris$Species, n_estimators = 4L)
#' emb <- tabpfn_embeddings(fit, iris[1:10, 1:4])
#' dim(emb)
#' }
#'
#' @export
tabpfn_embeddings <- function(object, new_data,
                               data_source = "test",
                               aggregate = "mean") {
  if (!inherits(object, c("tabpfn_classifier", "tabpfn_regressor"))) {
    rlang::abort(
      "object must be a fitted tabpfn_classifier or tabpfn_regressor.",
      class = "rtabpfn_input_error"
    )
  }
  valid_agg <- c("mean", "concat", "none")
  if (!aggregate %in% valid_agg) {
    rlang::abort(paste0(
      'aggregate must be one of: ',
      paste0('"', valid_agg, '"', collapse = ", "),
      '. Got: "', aggregate, '"'
    ))
  }
  valid_ds <- c("train", "test")
  if (!data_source %in% valid_ds) {
    rlang::abort(paste0(
      'data_source must be one of: ',
      paste0('"', valid_ds, '"', collapse = ", "),
      '. Got: "', data_source, '"'
    ))
  }

  x_np <- ensure_numpy_array(new_data)
  raw <- wrap_python_error(object$.py$get_embeddings(x_np, data_source))
  emb <- reticulate::py_to_r(raw)

  # emb shape: (n_estimators, n_samples, embedding_dim)
  switch(aggregate,
    mean = apply(emb, c(2, 3), mean),
    concat = {
      n_samp <- dim(emb)[2]
      matrix(aperm(emb, c(2, 1, 3)), nrow = n_samp)
    },
    none = emb
  )
}
