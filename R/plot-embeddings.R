#' Plot Embeddings with t-SNE or UMAP
#'
#' Reduces a high-dimensional embedding matrix to 2-D and produces a
#' ggplot2 scatter plot.
#'
#' @param embeddings A numeric matrix `(n_samples, embedding_dim)` as
#'   returned by [tabpfn_embeddings()] with `aggregate = "mean"` or
#'   `"concat"`.
#' @param labels Optional vector (factor, character, or numeric) of length
#'   `nrow(embeddings)` used to colour points.
#' @param method Character. `"tsne"` (default, requires \pkg{Rtsne}) or
#'   `"umap"` (requires \pkg{uwot}).
#' @param return_coords Logical. If `TRUE`, return a list with `$plot`
#'   (ggplot) and `$coords` (tibble). Default `FALSE`.
#' @param ... Additional arguments forwarded to [ggplot2::geom_point()]
#'   (e.g. `size`, `alpha`, `shape`).
#'
#' @return A ggplot2 object, or (when `return_coords = TRUE`) a list with
#'   elements `plot` and `coords`.
#'
#' @examples
#' \dontrun{
#' fit <- tabpfn_classifier(iris[, 1:4], iris$Species)
#' emb <- tabpfn_embeddings(fit, iris[, 1:4], data_source = "train")
#' plot_embeddings(emb, labels = iris$Species, method = "tsne")
#' plot_embeddings(emb, labels = iris$Species, method = "umap",
#'                 return_coords = TRUE)
#' }
#'
#' @export
plot_embeddings <- function(embeddings,
                             labels = NULL,
                             method = c("tsne", "umap"),
                             return_coords = FALSE,
                             ...) {
  method <- match.arg(method)

  if (!is.matrix(embeddings) || length(dim(embeddings)) != 2L) {
    rlang::abort(
      "embeddings must be a 2-D matrix. Use aggregate = 'mean' or 'concat'.",
      class = "rtabpfn_input_error"
    )
  }
  if (!is.null(labels) && length(labels) != nrow(embeddings)) {
    rlang::abort(
      paste0("labels length (", length(labels),
             ") must match nrow(embeddings) (", nrow(embeddings), ")."),
      class = "rtabpfn_input_error"
    )
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    rlang::abort("The ggplot2 package is required. Install with: install.packages('ggplot2')")
  }

  coords <- reduce_dims(embeddings, method)
  df <- tibble::tibble(dim1 = coords[, 1], dim2 = coords[, 2])
  if (!is.null(labels)) {
    df$label <- labels
  }

  axis_prefix <- if (method == "tsne") "t-SNE" else "UMAP"

  if (!is.null(labels)) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$dim1, y = .data$dim2,
                                           color = .data$label)) +
      ggplot2::geom_point(...) +
      ggplot2::labs(x = paste(axis_prefix, "1"),
                    y = paste(axis_prefix, "2"),
                    color = NULL) +
      ggplot2::theme_minimal()
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$dim1, y = .data$dim2)) +
      ggplot2::geom_point(...) +
      ggplot2::labs(x = paste(axis_prefix, "1"),
                    y = paste(axis_prefix, "2")) +
      ggplot2::theme_minimal()
  }

  if (return_coords) {
    list(plot = p, coords = df)
  } else {
    p
  }
}


#' Run t-SNE or UMAP on an embedding matrix
#' @keywords internal
reduce_dims <- function(embeddings, method) {
  if (method == "tsne") {
    if (!requireNamespace("Rtsne", quietly = TRUE)) {
      rlang::abort("The Rtsne package is required for method='tsne'. Install with: install.packages('Rtsne')")
    }
    perp <- min(30, floor((nrow(embeddings) - 1) / 3))
    res <- Rtsne::Rtsne(embeddings, dims = 2L, perplexity = perp,
                         check_duplicates = FALSE)
    res$Y
  } else {
    if (!requireNamespace("uwot", quietly = TRUE)) {
      rlang::abort("The uwot package is required for method='umap'. Install with: install.packages('uwot')")
    }
    uwot::umap(embeddings, n_components = 2L)
  }
}
