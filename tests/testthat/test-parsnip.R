test_that("tabpfn_model creates a valid parsnip model spec (classification)", {
  skip_if_not_installed("parsnip")
  spec <- tabpfn_model(mode = "classification", n_estimators = 4L)
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "classification")
  expect_equal(spec$engine, "tabpfn")
})

test_that("tabpfn_model creates a valid parsnip model spec (regression)", {
  skip_if_not_installed("parsnip")
  spec <- tabpfn_model(mode = "regression", n_estimators = 4L)
  expect_s3_class(spec, "model_spec")
  expect_equal(spec$mode, "regression")
})

test_that("tabpfn_model rejects invalid mode", {
  skip_if_not_installed("parsnip")
  expect_error(tabpfn_model(mode = "clustering"))
})

test_that("parsnip classification workflow fits and predicts", {
  skip_if_no_tabpfn()
  skip_if_not_installed("parsnip")
  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")

  spec <- tabpfn_model(mode = "classification", n_estimators = 2L,
                        device = "cpu")

  wf <- workflows::workflow() |>
    workflows::add_recipe(recipes::recipe(Species ~ ., data = iris)) |>
    workflows::add_model(spec)

  fit <- wf |> parsnip::fit(data = iris[1:120, ])

  preds <- predict(fit, iris[121:150, ])
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred_class")
  expect_equal(nrow(preds), 30L)

  probs <- predict(fit, iris[121:150, ], type = "prob")
  expect_s3_class(probs, "tbl_df")
  expect_equal(nrow(probs), 30L)
  expect_true(all(grepl("^\\.pred_", names(probs))))
})

test_that("parsnip regression workflow fits and predicts", {
  skip_if_no_tabpfn()
  skip_if_not_installed("parsnip")
  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")

  spec <- tabpfn_model(mode = "regression", n_estimators = 2L,
                        device = "cpu")

  wf <- workflows::workflow() |>
    workflows::add_recipe(recipes::recipe(mpg ~ cyl + disp + hp + wt,
                                          data = mtcars)) |>
    workflows::add_model(spec)

  fit <- wf |> parsnip::fit(data = mtcars[1:20, ])
  preds <- predict(fit, mtcars[21:32, ])
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred")
  expect_equal(nrow(preds), 12L)
})
