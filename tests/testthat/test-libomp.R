test_that("check_libomp runs without error on non-macOS", {
  skip_on_os("mac")
  expect_silent(check_libomp())
})

test_that("check_libomp is a function", {
  expect_true(is.function(check_libomp))
})
