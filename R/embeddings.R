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


#' Cross-Validated Embeddings from TabPFN
#'
#' Splits data into K folds, fits a fresh TabPFN on each training fold, and
#' extracts embeddings for the held-out validation fold. Each sample is
#' embedded by a model that was NOT trained on it.
#'
#' @param x A data.frame, tibble, data.table, or matrix of predictors.
#' @param y A factor/character (classification) or numeric (regression) vector.
#' @param n_folds Integer. Number of folds when `folds` is NULL. Default `5L`.
#' @param folds `NULL` (auto-split), an `rsample` rset object, or a list of
#'   `list(train = integer_idx, validation = integer_idx)`.
#' @param aggregate Character. `"mean"` (default), `"concat"`, or `"none"`.
#'   See [tabpfn_embeddings()].
#' @param mode Character. `"classification"` (default) or `"regression"`.
#' @param ... Additional arguments passed to [tabpfn_classifier()] or
#'   [tabpfn_regressor()].
#'
#' @return Same format as [tabpfn_embeddings()], with rows in original order.
#'
#' @examples
#' \dontrun{
#' emb <- tabpfn_embeddings_cv(iris[, 1:4], iris$Species, n_folds = 5)
#' dim(emb)
#' }
#'
#' @export
tabpfn_embeddings_cv <- function(x, y,
                                  n_folds = 5L,
                                  folds = NULL,
                                  aggregate = "mean",
                                  mode = "classification",
                                  ...) {
  n <- nrow(x)
  fold_list <- resolve_folds(folds, n_folds, n)

  results <- vector("list", length(fold_list))

  for (i in seq_along(fold_list)) {
    train_idx <- fold_list[[i]]$train
    val_idx   <- fold_list[[i]]$validation

    fit <- if (mode == "classification") {
      tabpfn_classifier(x[train_idx, , drop = FALSE], y[train_idx], ...)
    } else {
      tabpfn_regressor(x[train_idx, , drop = FALSE], y[train_idx], ...)
    }

    emb <- tabpfn_embeddings(
      fit, x[val_idx, , drop = FALSE],
      data_source = "test", aggregate = aggregate
    )
    results[[i]] <- list(idx = val_idx, emb = emb)
  }

  reassemble_cv_embeddings(results, n, aggregate)
}


#' Resolve fold specification into a list of train/validation index pairs
#' @keywords internal
resolve_folds <- function(folds, n_folds, n) {
  if (is.null(folds)) {
    fold_id <- sample(rep(seq_len(n_folds), length.out = n))
    lapply(seq_len(n_folds), function(k) {
      list(train = which(fold_id != k), validation = which(fold_id == k))
    })
  } else if (inherits(folds, "rset")) {
    lapply(folds$splits, function(split) {
      list(
        train      = as.integer(split, data = "analysis"),
        validation = as.integer(split, data = "assessment")
      )
    })
  } else if (is.list(folds)) {
    folds
  } else {
    rlang::abort(
      "folds must be NULL, an rsample rset, or a list of list(train, validation).",
      class = "rtabpfn_input_error"
    )
  }
}


#' Reassemble per-fold embeddings into original row order
#' @keywords internal
reassemble_cv_embeddings <- function(results, n, aggregate) {
  if (aggregate == "none") {
    n_est <- dim(results[[1]]$emb)[1]
    emb_dim <- dim(results[[1]]$emb)[3]
    out <- array(NA_real_, dim = c(n_est, n, emb_dim))
    for (r in results) {
      out[, r$idx, ] <- r$emb
    }
    out
  } else {
    emb_dim <- ncol(results[[1]]$emb)
    out <- matrix(NA_real_, nrow = n, ncol = emb_dim)
    for (r in results) {
      out[r$idx, ] <- r$emb
    }
    out
  }
}
