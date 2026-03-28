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
