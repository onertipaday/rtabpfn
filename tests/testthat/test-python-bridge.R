test_that("install_tabpfn is a function with expected defaults", {
  expect_true(is.function(install_tabpfn))
  args <- formals(install_tabpfn)
  expect_equal(args$envname, "r-tabpfn")
  expect_true(args$pip)
})

test_that("check_tabpfn_available returns TRUE when tabpfn is installed", {
  skip_if_no_tabpfn()
  expect_true(check_tabpfn_available())
})

test_that("check_tabpfn_available errors with helpful message when missing", {
  local_mocked_bindings(
    py_module_available = function(...) FALSE,
    .package = "reticulate"
  )
  expect_error(
    check_tabpfn_available(),
    class = "rtabpfn_missing_python"
  )
  expect_error(
    check_tabpfn_available(),
    "install_tabpfn"
  )
})

test_that("ensure_numpy_array converts data.frame to numpy", {
  skip_if_no_tabpfn()
  df <- data.frame(a = c(1.0, 2.0), b = c(3.0, 4.0))
  result <- ensure_numpy_array(df)
  expect_true(inherits(result, "numpy.ndarray"))
})

test_that("ensure_numpy_array converts matrix to numpy", {
  skip_if_no_tabpfn()
  mat <- matrix(c(1, 2, 3, 4), nrow = 2)
  result <- ensure_numpy_array(mat)
  expect_true(inherits(result, "numpy.ndarray"))
})

test_that("ensure_numpy_array converts tibble to numpy", {
  skip_if_no_tabpfn()
  tbl <- tibble::tibble(a = c(1.0, 2.0), b = c(3.0, 4.0))
  result <- ensure_numpy_array(tbl)
  expect_true(inherits(result, "numpy.ndarray"))
})

test_that("ensure_numpy_array converts data.table to numpy", {
  skip_if_no_tabpfn()
  skip_if_not_installed("data.table")
  dt <- data.table::data.table(a = c(1.0, 2.0), b = c(3.0, 4.0))
  result <- ensure_numpy_array(dt)
  expect_true(inherits(result, "numpy.ndarray"))
})

test_that("ensure_numpy_array encodes factors as integers", {
  skip_if_no_tabpfn()
  df <- data.frame(a = factor(c("cat", "dog", "cat")), b = c(1.0, 2.0, 3.0))
  result <- ensure_numpy_array(df)
  values <- reticulate::py_to_r(result)
  expect_equal(values[, 1], c(0, 1, 0))
})

test_that("ensure_numpy_array rejects unsupported types", {
  expect_error(
    ensure_numpy_array(list(a = 1)),
    class = "rtabpfn_input_error"
  )
})

test_that("validate_classifier_inputs coerces character to factor", {
  df <- data.frame(a = 1:3)
  result <- validate_classifier_inputs(df, c("a", "b", "a"))
  expect_true(is.factor(result))
  expect_equal(levels(result), c("a", "b"))
})

test_that("validate_classifier_inputs rejects numeric y", {
  df <- data.frame(a = 1:3)
  expect_error(
    validate_classifier_inputs(df, c(1.5, 2.5, 3.5)),
    class = "rtabpfn_input_error"
  )
})

test_that("validate_classifier_inputs rejects mismatched lengths", {
  df <- data.frame(a = 1:3)
  expect_error(
    validate_classifier_inputs(df, factor(c("a", "b"))),
    "same number"
  )
})

test_that("validate_regressor_inputs rejects non-numeric y", {
  df <- data.frame(a = 1:3)
  expect_error(
    validate_regressor_inputs(df, c("a", "b", "c")),
    class = "rtabpfn_input_error"
  )
})
