test_that("subsample_training_set returns NULL when data fits within limit", {
  x <- matrix(rnorm(100), nrow = 20, ncol = 5)
  y <- rnorm(20)
  expect_null(subsample_training_set(x, y, limit = 50))
  expect_null(subsample_training_set(x, y, limit = 20))
})

test_that("subsample_training_set returns correct number of indices for regression", {
  set.seed(42)
  x <- matrix(rnorm(500), nrow = 100, ncol = 5)
  y <- rnorm(100)
  idx <- subsample_training_set(x, y, limit = 30)
  expect_true(length(idx) <= 30)
  expect_true(length(idx) > 0)
  expect_true(all(idx >= 1 & idx <= 100))
  # Indices should be sorted

  expect_equal(idx, sort(idx))
})

test_that("subsample_training_set stratifies by class for factors", {
  set.seed(42)
  x <- matrix(rnorm(200), nrow = 100, ncol = 2)
  # 80 of class A, 20 of class B
  y <- factor(c(rep("A", 80), rep("B", 20)))
  idx <- subsample_training_set(x, y, limit = 50)
  expect_true(length(idx) <= 50)
  # Both classes should be represented
  sampled_classes <- y[idx]
  expect_true("A" %in% sampled_classes)
  expect_true("B" %in% sampled_classes)
  # Proportions should be roughly maintained
  prop_a <- sum(sampled_classes == "A") / length(idx)
  expect_true(prop_a > 0.5)  # A is majority
})

test_that("subsample_training_set handles edge case of limit = 1", {
  x <- matrix(rnorm(20), nrow = 10, ncol = 2)
  y <- rnorm(10)
  idx <- subsample_training_set(x, y, limit = 1)
  expect_equal(length(idx), 1)
})

test_that("subsample_training_set returns unique sorted indices", {
  set.seed(123)
  x <- matrix(rnorm(1000), nrow = 200, ncol = 5)
  y <- factor(sample(letters[1:5], 200, replace = TRUE))
  idx <- subsample_training_set(x, y, limit = 50)
  expect_equal(length(idx), length(unique(idx)))
  expect_equal(idx, sort(idx))
})
