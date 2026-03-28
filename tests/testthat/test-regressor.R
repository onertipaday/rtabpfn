test_that("tabpfn_regressor fits on mtcars and returns correct S3 class", {
  skip_if_no_tabpfn()
  x <- mtcars[, c("cyl", "disp", "hp", "wt")]
  y <- mtcars$mpg
  fit <- tabpfn_regressor(x, y, n_estimators = 2L, device = "cpu")
  expect_s3_class(fit, "tabpfn_regressor")
  expect_equal(fit$model_type, "regressor")
  expect_equal(fit$n_features, 4L)
  expect_equal(fit$n_train, 32L)
  expect_equal(fit$feature_names, c("cyl", "disp", "hp", "wt"))
})

test_that("tabpfn_regressor rejects non-numeric y", {
  x <- mtcars[, c("cyl", "disp")]
  expect_error(
    tabpfn_regressor(x, as.character(mtcars$mpg), device = "cpu"),
    class = "rtabpfn_input_error"
  )
})

test_that("predict.tabpfn_regressor returns numeric predictions", {
  skip_if_no_tabpfn()
  x_train <- mtcars[1:20, c("cyl", "disp", "hp", "wt")]
  y_train <- mtcars$mpg[1:20]
  x_test <- mtcars[21:32, c("cyl", "disp", "hp", "wt")]
  fit <- tabpfn_regressor(x_train, y_train, n_estimators = 2L, device = "cpu")
  preds <- predict(fit, x_test)
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred")
  expect_equal(nrow(preds), 12L)
  expect_true(is.double(preds$.pred))
})

test_that("predict.tabpfn_regressor returns median predictions", {
  skip_if_no_tabpfn()
  x_train <- mtcars[1:20, c("cyl", "disp", "hp", "wt")]
  y_train <- mtcars$mpg[1:20]
  x_test <- mtcars[21:32, c("cyl", "disp", "hp", "wt")]
  fit <- tabpfn_regressor(x_train, y_train, n_estimators = 2L, device = "cpu")
  preds <- predict(fit, x_test, type = "median")
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred")
  expect_equal(nrow(preds), 12L)
})

test_that("predict.tabpfn_regressor returns mode predictions", {
  skip_if_no_tabpfn()
  x_train <- mtcars[1:20, c("cyl", "disp", "hp", "wt")]
  y_train <- mtcars$mpg[1:20]
  x_test <- mtcars[21:32, c("cyl", "disp", "hp", "wt")]
  fit <- tabpfn_regressor(x_train, y_train, n_estimators = 2L, device = "cpu")
  preds <- predict(fit, x_test, type = "mode")
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred")
  expect_equal(nrow(preds), 12L)
})

test_that("predict.tabpfn_regressor returns quantile predictions", {
  skip_if_no_tabpfn()
  x_train <- mtcars[1:20, c("cyl", "disp", "hp", "wt")]
  y_train <- mtcars$mpg[1:20]
  x_test <- mtcars[21:32, c("cyl", "disp", "hp", "wt")]
  fit <- tabpfn_regressor(x_train, y_train, n_estimators = 2L, device = "cpu")
  preds <- predict(fit, x_test, type = "quantile", quantiles = c(0.1, 0.5, 0.9))
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, c(".row", ".quantile", ".pred"))
  expect_equal(nrow(preds), 36L)
  expect_equal(sort(unique(preds$.quantile)), c(0.1, 0.5, 0.9))
})

test_that("predict.tabpfn_regressor errors when quantile type without quantiles param", {
  skip_if_no_tabpfn()
  x_train <- mtcars[1:20, c("cyl", "disp", "hp", "wt")]
  y_train <- mtcars$mpg[1:20]
  fit <- tabpfn_regressor(x_train, y_train, n_estimators = 2L, device = "cpu")
  expect_error(predict(fit, mtcars[21:25, c("cyl", "disp", "hp", "wt")],
                       type = "quantile"))
})

test_that("print.tabpfn_regressor produces output", {
  skip_if_no_tabpfn()
  fit <- tabpfn_regressor(
    mtcars[, c("cyl", "disp")], mtcars$mpg,
    n_estimators = 2L, device = "cpu"
  )
  expect_output(print(fit), "TabPFN Regressor")
  expect_output(print(fit), "2 features")
})
