test_that("tabpfn_classifier fits on iris and returns correct S3 class", {
  skip_if_no_tabpfn()
  fit <- tabpfn_classifier(
    iris[, 1:4], iris$Species,
    n_estimators = 2L, device = "cpu"
  )
  expect_s3_class(fit, "tabpfn_classifier")
  expect_equal(fit$model_type, "classifier")
  expect_equal(fit$n_classes, 3L)
  expect_equal(fit$classes, levels(iris$Species))
  expect_equal(fit$n_features, 4L)
  expect_equal(fit$n_train, 150L)
  expect_equal(fit$feature_names, c("Sepal.Length", "Sepal.Width",
                                     "Petal.Length", "Petal.Width"))
})

test_that("tabpfn_classifier accepts character y", {
  skip_if_no_tabpfn()
  fit <- tabpfn_classifier(
    iris[, 1:4], as.character(iris$Species),
    n_estimators = 2L, device = "cpu"
  )
  expect_s3_class(fit, "tabpfn_classifier")
  expect_equal(fit$n_classes, 3L)
})

test_that("tabpfn_classifier rejects numeric y", {
  expect_error(
    tabpfn_classifier(iris[, 1:4], iris$Sepal.Length, device = "cpu"),
    class = "rtabpfn_input_error"
  )
})

test_that("tabpfn_classifier stores params correctly", {
  skip_if_no_tabpfn()
  fit <- tabpfn_classifier(
    iris[, 1:4], iris$Species,
    n_estimators = 4L, softmax_temperature = 0.8,
    balance_probabilities = TRUE, device = "cpu"
  )
  expect_equal(fit$params$n_estimators, 4L)
  expect_equal(fit$params$softmax_temperature, 0.8)
  expect_true(fit$params$balance_probabilities)
  expect_equal(fit$params$device, "cpu")
})
