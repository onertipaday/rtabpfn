test_that("plot_embeddings returns ggplot with tsne", {
  skip_if_no_tabpfn()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("Rtsne")

  fit <- tabpfn_classifier(
    iris[1:120, 1:4], iris$Species[1:120],
    n_estimators = 2L, device = "cpu"
  )
  emb <- tabpfn_embeddings(fit, iris[1:120, 1:4], data_source = "train")
  p <- plot_embeddings(emb, labels = iris$Species[1:120], method = "tsne")
  expect_s3_class(p, "ggplot")
})

test_that("plot_embeddings returns ggplot with umap", {
  skip_if_no_tabpfn()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("uwot")

  fit <- tabpfn_classifier(
    iris[1:120, 1:4], iris$Species[1:120],
    n_estimators = 2L, device = "cpu"
  )
  emb <- tabpfn_embeddings(fit, iris[1:120, 1:4], data_source = "train")
  p <- plot_embeddings(emb, labels = iris$Species[1:120], method = "umap")
  expect_s3_class(p, "ggplot")
})

test_that("plot_embeddings works without labels", {
  skip_if_no_tabpfn()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("Rtsne")

  fit <- tabpfn_classifier(
    iris[1:120, 1:4], iris$Species[1:120],
    n_estimators = 2L, device = "cpu"
  )
  emb <- tabpfn_embeddings(fit, iris[1:120, 1:4], data_source = "train")
  p <- plot_embeddings(emb, method = "tsne")
  expect_s3_class(p, "ggplot")
})

test_that("plot_embeddings with return_coords returns list", {
  skip_if_no_tabpfn()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("Rtsne")

  fit <- tabpfn_classifier(
    iris[1:120, 1:4], iris$Species[1:120],
    n_estimators = 2L, device = "cpu"
  )
  emb <- tabpfn_embeddings(fit, iris[1:120, 1:4], data_source = "train")
  res <- plot_embeddings(emb, labels = iris$Species[1:120],
                         method = "tsne", return_coords = TRUE)
  expect_type(res, "list")
  expect_s3_class(res$plot, "ggplot")
  expect_s3_class(res$coords, "tbl_df")
  expect_true("dim1" %in% names(res$coords))
  expect_true("dim2" %in% names(res$coords))
  expect_true("label" %in% names(res$coords))
  expect_equal(nrow(res$coords), 120L)
})

test_that("plot_embeddings return_coords without labels omits label column", {
  skip_if_no_tabpfn()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("uwot")

  fit <- tabpfn_classifier(
    iris[1:120, 1:4], iris$Species[1:120],
    n_estimators = 2L, device = "cpu"
  )
  emb <- tabpfn_embeddings(fit, iris[1:120, 1:4], data_source = "train")
  res <- plot_embeddings(emb, method = "umap", return_coords = TRUE)
  expect_false("label" %in% names(res$coords))
})

test_that("plot_embeddings rejects mismatched labels length", {
  skip_if_no_tabpfn()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("Rtsne")

  fit <- tabpfn_classifier(
    iris[1:120, 1:4], iris$Species[1:120],
    n_estimators = 2L, device = "cpu"
  )
  emb <- tabpfn_embeddings(fit, iris[1:120, 1:4], data_source = "train")
  expect_error(
    plot_embeddings(emb, labels = iris$Species[1:10], method = "tsne"),
    "labels"
  )
})

test_that("plot_embeddings rejects 3D array input", {
  skip_if_no_tabpfn()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("Rtsne")

  fit <- tabpfn_classifier(
    iris[1:120, 1:4], iris$Species[1:120],
    n_estimators = 2L, device = "cpu"
  )
  emb_3d <- tabpfn_embeddings(fit, iris[1:120, 1:4],
                               data_source = "train", aggregate = "none")
  expect_error(
    plot_embeddings(emb_3d, method = "tsne"),
    "2-D matrix"
  )
})
