test_that("augment.tabpfn_classifier returns data with predictions", {
  skip_if_no_tabpfn()
  fit <- tabpfn_classifier(
    iris[1:120, 1:4], iris$Species[1:120],
    n_estimators = 2L, device = "cpu"
  )
  result <- augment(fit, iris[121:150, 1:4])
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 30L)
  # Should have original columns + .pred_class + probability columns
  expect_true(".pred_class" %in% names(result))
  expect_true(".pred_setosa" %in% names(result))
  expect_true(".pred_versicolor" %in% names(result))
  expect_true(".pred_virginica" %in% names(result))
  # Original columns preserved
  expect_true("Sepal.Length" %in% names(result))
})

test_that("augment.tabpfn_regressor returns data with predictions", {
  skip_if_no_tabpfn()
  fit <- tabpfn_regressor(
    mtcars[1:25, -1], mtcars$mpg[1:25],
    n_estimators = 2L, device = "cpu"
  )
  result <- augment(fit, mtcars[26:32, -1])
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 7L)
  expect_true(".pred" %in% names(result))
  # Original columns preserved
  expect_true("cyl" %in% names(result))
})
