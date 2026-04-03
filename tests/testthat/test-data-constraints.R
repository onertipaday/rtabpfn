test_that("check_data_constraints errors on too many rows", {
  big_x <- matrix(0, nrow = 50001, ncol = 5)
  y <- rnorm(50001)
  expect_error(
    check_data_constraints(big_x, y),
    class = "rtabpfn_data_constraint_error"
  )
  expect_error(
    check_data_constraints(big_x, y),
    "50,001 rows"
  )
})

test_that("check_data_constraints errors on too many columns", {
  wide_x <- matrix(0, nrow = 10, ncol = 2001)
  y <- rnorm(10)
  expect_error(
    check_data_constraints(wide_x, y),
    class = "rtabpfn_data_constraint_error"
  )
  expect_error(
    check_data_constraints(wide_x, y),
    "2,001 predictors"
  )
})

test_that("check_data_constraints errors on too many classes", {
  x <- matrix(rnorm(22), nrow = 11, ncol = 2)
  y <- factor(paste0("class_", 1:11))
  expect_error(
    check_data_constraints(x, y),
    class = "rtabpfn_data_constraint_error"
  )
  expect_error(
    check_data_constraints(x, y),
    "11 classes"
  )
})

test_that("check_data_constraints class limit cannot be bypassed", {
  x <- matrix(rnorm(22), nrow = 11, ncol = 2)
  y <- factor(paste0("class_", 1:11))
  expect_error(
    check_data_constraints(x, y, ignore_limits = TRUE),
    class = "rtabpfn_data_constraint_error"
  )
})

test_that("check_data_constraints respects ignore_limits for rows/cols", {
  big_x <- matrix(0, nrow = 50001, ncol = 5)
  y <- rnorm(50001)
  expect_silent(check_data_constraints(big_x, y, ignore_limits = TRUE))

  wide_x <- matrix(0, nrow = 10, ncol = 2001)
  y2 <- rnorm(10)
  expect_silent(check_data_constraints(wide_x, y2, ignore_limits = TRUE))
})

test_that("check_data_constraints passes for valid data", {
  expect_silent(check_data_constraints(iris[, 1:4], iris$Species))
  expect_silent(check_data_constraints(mtcars[, -1], mtcars$mpg))
})
