test_that("control_tabpfn creates correct structure", {
  ctrl <- control_tabpfn()
  expect_s3_class(ctrl, "control_tabpfn")
  expect_equal(ctrl$n_preprocessing_jobs, 1L)
  expect_equal(ctrl$fit_mode, "fit_preprocessors")
  expect_equal(ctrl$memory_saving_mode, "auto")
  expect_equal(ctrl$inference_precision, "auto")
})

test_that("control_tabpfn accepts valid fit_mode values", {
  for (mode in c("fit_preprocessors", "low_memory", "fit_with_cache", "batched")) {
    ctrl <- control_tabpfn(fit_mode = mode)
    expect_equal(ctrl$fit_mode, mode)
  }
})

test_that("control_tabpfn rejects invalid fit_mode", {
  expect_error(
    control_tabpfn(fit_mode = "invalid"),
    class = "rtabpfn_input_error"
  )
})

test_that("control_tabpfn accepts custom values", {
  ctrl <- control_tabpfn(
    n_preprocessing_jobs = -1L,
    fit_mode = "low_memory",
    memory_saving_mode = TRUE,
    inference_precision = "autocast"
  )
  expect_equal(ctrl$n_preprocessing_jobs, -1L)
  expect_equal(ctrl$fit_mode, "low_memory")
  expect_true(ctrl$memory_saving_mode)
  expect_equal(ctrl$inference_precision, "autocast")
})

test_that("print.control_tabpfn produces output", {
  ctrl <- control_tabpfn()
  expect_output(print(ctrl), "TabPFN control options")
})

test_that("print.control_tabpfn marks non-default values", {
  ctrl <- control_tabpfn(fit_mode = "low_memory")
  out <- capture.output(print(ctrl))
  # The low_memory line should have an asterisk marker
  low_mem_line <- grep("fit_mode", out, value = TRUE)
  expect_true(grepl("\\*", low_mem_line))
})
