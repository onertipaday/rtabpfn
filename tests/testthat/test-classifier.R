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

test_that("predict.tabpfn_classifier returns class predictions as tibble", {
  skip_if_no_tabpfn()
  fit <- tabpfn_classifier(
    iris[1:100, 1:4], iris$Species[1:100],
    n_estimators = 2L, device = "cpu"
  )
  preds <- predict(fit, iris[101:150, 1:4])
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred_class")
  expect_s3_class(preds$.pred_class, "factor")
  expect_equal(levels(preds$.pred_class), levels(iris$Species))
  expect_equal(nrow(preds), 50L)
})

test_that("predict.tabpfn_classifier returns probabilities as tibble", {
  skip_if_no_tabpfn()
  fit <- tabpfn_classifier(
    iris[1:100, 1:4], iris$Species[1:100],
    n_estimators = 2L, device = "cpu"
  )
  probs <- predict(fit, iris[101:150, 1:4], type = "prob")
  expect_s3_class(probs, "tbl_df")
  expect_named(probs, c(".pred_setosa", ".pred_versicolor", ".pred_virginica"))
  expect_equal(nrow(probs), 50L)
  row_sums <- rowSums(as.matrix(probs))
  expect_true(all(abs(row_sums - 1.0) < 0.01))
})

test_that("predict.tabpfn_classifier rejects invalid type", {
  skip_if_no_tabpfn()
  fit <- tabpfn_classifier(
    iris[, 1:4], iris$Species,
    n_estimators = 2L, device = "cpu"
  )
  expect_error(predict(fit, iris[1:5, 1:4], type = "invalid"))
})

test_that("print.tabpfn_classifier produces output", {
  skip_if_no_tabpfn()
  fit <- tabpfn_classifier(
    iris[, 1:4], iris$Species,
    n_estimators = 2L, device = "cpu"
  )
  expect_output(print(fit), "TabPFN Classifier")
  expect_output(print(fit), "3 classes")
  expect_output(print(fit), "4 features")
})
