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

# --- tabpfn_embeddings_cv ---------------------------------------------------

test_that("tabpfn_embeddings_cv returns embeddings for all samples (classifier)", {
  skip_if_no_tabpfn()
  emb <- tabpfn_embeddings_cv(
    iris[1:120, 1:4], iris$Species[1:120],
    n_folds = 3L, mode = "classification",
    n_estimators = 2L, device = "cpu"
  )
  expect_true(is.matrix(emb))
  expect_equal(nrow(emb), 120L)
  expect_true(ncol(emb) > 0L)
})

test_that("tabpfn_embeddings_cv works for regression", {
  skip_if_no_tabpfn()
  emb <- tabpfn_embeddings_cv(
    mtcars[, c("cyl", "disp", "hp", "wt")], mtcars$mpg,
    n_folds = 3L, mode = "regression",
    n_estimators = 2L, device = "cpu"
  )
  expect_true(is.matrix(emb))
  expect_equal(nrow(emb), 32L)
})

test_that("tabpfn_embeddings_cv accepts rsample folds", {
  skip_if_no_tabpfn()
  skip_if_not_installed("rsample")
  folds <- rsample::vfold_cv(iris[1:120, ], v = 3)
  emb <- tabpfn_embeddings_cv(
    iris[1:120, 1:4], iris$Species[1:120],
    folds = folds, mode = "classification",
    n_estimators = 2L, device = "cpu"
  )
  expect_true(is.matrix(emb))
  expect_equal(nrow(emb), 120L)
})

test_that("tabpfn_embeddings_cv accepts list-of-indices folds", {
  skip_if_no_tabpfn()
  n <- 120L
  idx <- sample(rep(1:3, length.out = n))
  custom_folds <- lapply(1:3, function(k) {
    list(train = which(idx != k), validation = which(idx == k))
  })
  emb <- tabpfn_embeddings_cv(
    iris[1:n, 1:4], iris$Species[1:n],
    folds = custom_folds, mode = "classification",
    n_estimators = 2L, device = "cpu"
  )
  expect_true(is.matrix(emb))
  expect_equal(nrow(emb), n)
})

test_that("tabpfn_embeddings_cv respects aggregate parameter", {
  skip_if_no_tabpfn()
  emb_concat <- tabpfn_embeddings_cv(
    iris[1:120, 1:4], iris$Species[1:120],
    n_folds = 3L, aggregate = "concat", mode = "classification",
    n_estimators = 2L, device = "cpu"
  )
  emb_mean <- tabpfn_embeddings_cv(
    iris[1:120, 1:4], iris$Species[1:120],
    n_folds = 3L, aggregate = "mean", mode = "classification",
    n_estimators = 2L, device = "cpu"
  )
  expect_equal(ncol(emb_concat), 2L * ncol(emb_mean))
})

# --- extract_embeddings (parsnip / workflows) --------------------------------

test_that("extract_embeddings works on a workflow fit", {
  skip_if_no_tabpfn()
  skip_if_not_installed("parsnip")
  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")

  spec <- tabpfn_model(mode = "classification", n_estimators = 2L, device = "cpu")
  rec <- recipes::recipe(Species ~ ., data = iris[1:120, ])
  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(spec)
  wf_fit <- parsnip::fit(wf, data = iris[1:120, ])

  emb <- extract_embeddings(wf_fit, iris[121:150, 1:4])
  expect_true(is.matrix(emb))
  expect_equal(nrow(emb), 30L)
})

test_that("extract_embeddings forwards aggregate parameter", {
  skip_if_no_tabpfn()
  skip_if_not_installed("parsnip")
  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")

  spec <- tabpfn_model(mode = "classification", n_estimators = 2L, device = "cpu")
  rec <- recipes::recipe(Species ~ ., data = iris[1:120, ])
  wf <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(spec)
  wf_fit <- parsnip::fit(wf, data = iris[1:120, ])

  emb_none <- extract_embeddings(wf_fit, iris[121:150, 1:4], aggregate = "none")
  expect_equal(length(dim(emb_none)), 3L)
})
