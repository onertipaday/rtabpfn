test_that("tabpfn_embeddings returns matrix with aggregate='mean' (classifier)", {
  skip_if_no_tabpfn()
  fit <- tabpfn_classifier(
    iris[1:120, 1:4], iris$Species[1:120],
    n_estimators = 2L, device = "cpu"
  )
  emb <- tabpfn_embeddings(fit, iris[121:150, 1:4], aggregate = "mean")
  expect_true(is.matrix(emb))
  expect_equal(nrow(emb), 30L)
  expect_true(ncol(emb) > 0L)
})

test_that("tabpfn_embeddings returns wider matrix with aggregate='concat'", {
  skip_if_no_tabpfn()
  fit <- tabpfn_classifier(
    iris[1:120, 1:4], iris$Species[1:120],
    n_estimators = 2L, device = "cpu"
  )
  emb_mean   <- tabpfn_embeddings(fit, iris[121:150, 1:4], aggregate = "mean")
  emb_concat <- tabpfn_embeddings(fit, iris[121:150, 1:4], aggregate = "concat")
  expect_true(is.matrix(emb_concat))
  expect_equal(nrow(emb_concat), 30L)
  expect_equal(ncol(emb_concat), 2L * ncol(emb_mean))
})

test_that("tabpfn_embeddings returns 3D array with aggregate='none'", {
  skip_if_no_tabpfn()
  fit <- tabpfn_classifier(
    iris[1:120, 1:4], iris$Species[1:120],
    n_estimators = 2L, device = "cpu"
  )
  emb <- tabpfn_embeddings(fit, iris[121:150, 1:4], aggregate = "none")
  expect_true(is.array(emb))
  expect_equal(length(dim(emb)), 3L)
  expect_equal(dim(emb)[1], 2L)
  expect_equal(dim(emb)[2], 30L)
})

test_that("tabpfn_embeddings works with regressor", {
  skip_if_no_tabpfn()
  x <- mtcars[1:20, c("cyl", "disp", "hp", "wt")]
  y <- mtcars$mpg[1:20]
  fit <- tabpfn_regressor(x, y, n_estimators = 2L, device = "cpu")
  emb <- tabpfn_embeddings(fit, mtcars[21:32, c("cyl", "disp", "hp", "wt")])
  expect_true(is.matrix(emb))
  expect_equal(nrow(emb), 12L)
  expect_true(ncol(emb) > 0L)
})

test_that("tabpfn_embeddings respects data_source='train'", {
  skip_if_no_tabpfn()
  fit <- tabpfn_classifier(
    iris[1:120, 1:4], iris$Species[1:120],
    n_estimators = 2L, device = "cpu"
  )
  emb_train <- tabpfn_embeddings(fit, iris[1:120, 1:4], data_source = "train")
  expect_true(is.matrix(emb_train))
  expect_equal(nrow(emb_train), 120L)
})

test_that("tabpfn_embeddings rejects non-tabpfn objects", {
  expect_error(
    tabpfn_embeddings(lm(mpg ~ cyl, data = mtcars), mtcars[1:5, ]),
    class = "rtabpfn_input_error"
  )
})

test_that("tabpfn_embeddings rejects invalid aggregate", {
  skip_if_no_tabpfn()
  fit <- tabpfn_classifier(
    iris[1:120, 1:4], iris$Species[1:120],
    n_estimators = 2L, device = "cpu"
  )
  expect_error(
    tabpfn_embeddings(fit, iris[121:150, 1:4], aggregate = "invalid"),
    "aggregate"
  )
})
