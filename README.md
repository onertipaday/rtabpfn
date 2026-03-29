# rtabpfn

<!-- badges: start -->
[![R-CMD-check](https://github.com/onertipaday/rtabpfn/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/onertipaday/rtabpfn/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**rtabpfn** is an R interface to [TabPFN](https://github.com/PriorLabs/TabPFN) (Tabular Prior-Data Fitted Networks), a transformer-based in-context learning model for tabular data developed by [PriorLabs](https://priorlabs.ai/). It supports classification and regression with no hyperparameter tuning, and integrates with the [tidymodels](https://www.tidymodels.org/) ecosystem via a custom parsnip engine.

> Built with PriorLabs-TabPFN

## Features

- Fit TabPFN classifiers and regressors with a scikit-learn-style R API
- Native tibble input/output; works with data.frame, tibble, data.table, and matrix
- Full tidymodels integration: use `tabpfn_model()` inside any `workflow()`
- Quantile regression and uncertainty intervals out of the box
- Automatic Python environment management via reticulate

## Installation

### 1. Install the R package

```r
# From GitHub (recommended)
pak::pak("onertipaday/rtabpfn")

# Or with remotes
remotes::install_github("onertipaday/rtabpfn")
```

### 2. Install the Python backend

```r
library(rtabpfn)
install_tabpfn()   # creates a "r-tabpfn" virtualenv and installs tabpfn
```

If you prefer to manage the Python environment yourself:

```bash
pip install tabpfn
```

### System requirements

- R >= 4.1
- Python >= 3.9 with the `tabpfn` package
- GPU (CUDA) strongly recommended for datasets > 1000 rows; CPU is fine for smaller datasets

## Quick start

### Classification

```r
library(rtabpfn)
library(rsample)

set.seed(42)
split   <- initial_split(iris, prop = 0.8, strata = Species)
X_train <- training(split)[, 1:4]
y_train <- training(split)$Species
X_test  <- testing(split)[, 1:4]

# Fit
clf <- tabpfn_classifier(X_train, y_train, n_estimators = 8L, device = "auto")
clf

# Predict classes
predict(clf, X_test)

# Predict class probabilities
predict(clf, X_test, type = "prob")
```

### Regression

```r
library(rtabpfn)

set.seed(42)
idx   <- sample(nrow(mtcars), 24)
train <- mtcars[idx, ]
test  <- mtcars[-idx, ]

reg <- tabpfn_regressor(train[, -1], train$mpg, n_estimators = 8L, device = "auto")

# Point predictions
predict(reg, test[, -1])

# Quantile predictions (uncertainty intervals)
predict(reg, test[, -1], type = "quantile", quantiles = c(0.1, 0.5, 0.9))
```

### tidymodels / parsnip workflow

```r
library(rtabpfn)
library(tidymodels)

split  <- initial_split(iris, prop = 0.8, strata = Species)
spec   <- tabpfn_model(mode = "classification", n_estimators = 8L)

wf <- workflow() |>
  add_recipe(recipe(Species ~ ., data = training(split))) |>
  add_model(spec)

fit   <- fit(wf, data = training(split))
preds <- predict(fit, testing(split), type = "prob")
```

## Vignettes

| Vignette | Description |
|---|---|
| [Getting Started](vignettes/getting-started.Rmd) | Core API: classification and regression |
| [tidymodels Integration](vignettes/tidymodels-integration.Rmd) | Workflows, cross-validation, and tuning |
| [TabPFN Demo](vignettes/tabpfn-demo.Rmd) | R translation of the official Python hands-on notebook |

## About TabPFN

TabPFN is a foundation model for tabular data trained on synthetic datasets via meta-learning. It performs in-context learning: the training set is passed as part of the forward pass, so there is no gradient-based fitting at inference time. This makes it extremely fast and competitive with gradient-boosted trees on small-to-medium datasets (< 100k rows, < 2000 features).

- Paper: [Hollmann et al., 2025 — TabPFN v2](https://arxiv.org/abs/2501.02945)
- Python package: [github.com/PriorLabs/TabPFN](https://github.com/PriorLabs/TabPFN)
- License: [Prior Labs License (Apache 2.0 + attribution)](https://github.com/PriorLabs/TabPFN/blob/main/LICENSE)

## License

rtabpfn is MIT licensed. See [LICENSE](LICENSE).

rtabpfn does not redistribute TabPFN source code or model weights. The TabPFN Python package is installed separately and called at runtime via [reticulate](https://rstudio.github.io/reticulate/). Use of TabPFN is subject to the [Prior Labs License](https://github.com/PriorLabs/TabPFN/blob/main/LICENSE), which requires the attribution "Built with PriorLabs-TabPFN" when distributing products or services that use TabPFN.

## Authorship

The code in this package was written primarily by [Claude](https://claude.ai) (Anthropic) and [Codex](https://openai.com) (OpenAI). The project was conceived, directed, and validated by [Paolo Sonego](https://github.com/onertipaday).
