test_that("tabpfn_classifier works with formula interface", {
  skip_if_no_tabpfn()
  fit <- tabpfn_classifier(Species ~ ., data = iris, n_estimators = 2L, device = "cpu")
  expect_s3_class(fit, "tabpfn_classifier")
  expect_equal(fit$n_classes, 3L)
  expect_equal(fit$n_train, 150L)
  expect_equal(fit$n_features, 4L)
})

test_that("tabpfn_classifier formula predict works with full data frame", {
  skip_if_no_tabpfn()
  fit <- tabpfn_classifier(Species ~ ., data = iris[1:120, ],
                           n_estimators = 2L, device = "cpu")
  # Predict with data that includes the outcome column
  preds <- predict(fit, iris[121:150, ])
  expect_s3_class(preds, "tbl_df")
  expect_equal(nrow(preds), 30L)
  expect_named(preds, ".pred_class")
})

test_that("tabpfn_classifier formula works with subset of predictors", {
  skip_if_no_tabpfn()
  fit <- tabpfn_classifier(Species ~ Sepal.Length + Sepal.Width,
                           data = iris, n_estimators = 2L, device = "cpu")
  expect_equal(fit$n_features, 2L)
  expect_equal(fit$feature_names, c("Sepal.Length", "Sepal.Width"))
})

test_that("tabpfn_regressor works with formula interface", {
  skip_if_no_tabpfn()
  fit <- tabpfn_regressor(mpg ~ ., data = mtcars, n_estimators = 2L, device = "cpu")
  expect_s3_class(fit, "tabpfn_regressor")
  expect_equal(fit$n_train, 32L)
  expect_equal(fit$n_features, 10L)
})

test_that("tabpfn_regressor formula predict works with full data frame", {
  skip_if_no_tabpfn()
  fit <- tabpfn_regressor(mpg ~ ., data = mtcars[1:25, ],
                          n_estimators = 2L, device = "cpu")
  # Predict with data that includes the outcome column
  preds <- predict(fit, mtcars[26:32, ])
  expect_s3_class(preds, "tbl_df")
  expect_equal(nrow(preds), 7L)
  expect_named(preds, ".pred")
})

test_that("tabpfn_regressor formula works with subset of predictors", {
  skip_if_no_tabpfn()
  fit <- tabpfn_regressor(mpg ~ cyl + disp + hp,
                          data = mtcars, n_estimators = 2L, device = "cpu")
  expect_equal(fit$n_features, 3L)
  expect_equal(fit$feature_names, c("cyl", "disp", "hp"))
})
