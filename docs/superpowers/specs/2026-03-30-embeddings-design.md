# Embeddings & Visualization for rtabpfn

**Date:** 2026-03-30
**Status:** Approved

## Summary

Add embedding extraction (vanilla and cross-validated) and dimensionality-reduction visualization (t-SNE, UMAP) to the rtabpfn package. This exposes TabPFN's internal representations as a first-class feature with a clean R API, parsnip integration, and ready-made plots.

## Motivation

TabPFN's transformer produces dense embeddings that capture learned relationships in the data. These are useful for exploratory analysis, transfer learning, and understanding model behavior. The Python API already exposes `get_embeddings()` on both classifier and regressor; rtabpfn currently wraps only predictions.

## Design

### 1. Embedding Extraction -- `R/embeddings.R`

#### `tabpfn_embeddings(object, new_data, data_source, aggregate)`

Extract embeddings from a fitted TabPFN model.

| Parameter     | Type / Default                        | Description                                                    |
|---------------|---------------------------------------|----------------------------------------------------------------|
| `object`      | `tabpfn_classifier` or `tabpfn_regressor` | A fitted TabPFN model                                     |
| `new_data`    | data.frame / tibble / matrix          | Observations to embed                                          |
| `data_source` | `"test"` (default) or `"train"`       | Which transformer tokens to return                             |
| `aggregate`   | `"mean"` (default), `"concat"`, `"none"` | How to aggregate across estimators                          |

**Returns:**
- `aggregate = "mean"`: matrix `(n_samples, embedding_dim)` -- averaged over estimators
- `aggregate = "concat"`: matrix `(n_samples, n_estimators * embedding_dim)` -- concatenated
- `aggregate = "none"`: 3D array `(n_estimators, n_samples, embedding_dim)` -- raw

**Implementation:** Calls `object$.py$get_embeddings(x_np, data_source)` via reticulate, converts with `py_to_r`, then aggregates in R.

#### `tabpfn_embeddings_cv(x, y, n_folds, folds, aggregate, mode, ...)`

Cross-validated embeddings: each sample gets embedded by a model that was NOT trained on it.

| Parameter   | Type / Default                          | Description                                                   |
|-------------|------------------------------------------|---------------------------------------------------------------|
| `x`         | data.frame / tibble / matrix             | Full predictor matrix                                         |
| `y`         | factor (classification) or numeric (regression) | Full response vector                                  |
| `n_folds`   | integer, default `5L`                    | Number of folds (used when `folds` is NULL)                   |
| `folds`     | NULL, rsample rset, or list of index pairs | Custom fold specification; overrides `n_folds`              |
| `aggregate` | `"mean"` / `"concat"` / `"none"`        | Same as `tabpfn_embeddings`                                   |
| `mode`      | `"classification"` (default) or `"regression"` | Which TabPFN estimator to use                           |
| `...`       | named args                               | Forwarded to `tabpfn_classifier()` or `tabpfn_regressor()` (n_estimators, device, etc.) |

**Returns:** Same format as `tabpfn_embeddings`, with rows in original sample order.

**Algorithm:**
1. If `folds` is NULL, create K-fold split from `n_folds` (stratified for classification via `sample()` + modular assignment, or use rsample if available).
2. If `folds` is an rsample rset, extract train/validation indices from each split.
3. If `folds` is a list, expect `list(train = integer_idx, validation = integer_idx)` per element.
4. For each fold: fit a fresh TabPFN on training indices, call `tabpfn_embeddings()` with `data_source = "test"` on validation indices.
5. Reassemble validation embeddings into original row order.

#### `extract_embeddings(object, new_data, ...)`

Convenience wrapper for parsnip / workflow fits.

| Parameter  | Type                                 | Description                       |
|------------|--------------------------------------|-----------------------------------|
| `object`   | parsnip model_fit or workflow fit     | A fitted parsnip/workflow object  |
| `new_data` | data.frame / tibble / matrix          | Observations to embed             |
| `...`      | named args                            | Forwarded to `tabpfn_embeddings()`|

**Implementation:**
- If workflow: `workflows::extract_fit_parsnip(object)` then `parsnip::extract_fit_engine()`.
- If parsnip model_fit: `parsnip::extract_fit_engine()` directly.
- Calls `tabpfn_embeddings()` on the extracted engine object.

### 2. Visualization -- `R/plot-embeddings.R`

#### `plot_embeddings(embeddings, labels, method, return_coords, ...)`

Dimensionality reduction + ggplot2 visualization.

| Parameter      | Type / Default                          | Description                                         |
|----------------|-----------------------------------------|-----------------------------------------------------|
| `embeddings`   | matrix `(n_samples, embedding_dim)`     | 2D embedding matrix from `tabpfn_embeddings()`      |
| `labels`       | NULL (default), factor/character/numeric | Coloring variable; length must match nrow           |
| `method`       | `"tsne"` (default) or `"umap"`          | Dimensionality reduction algorithm                  |
| `return_coords`| FALSE (default)                         | If TRUE, return list with `$plot` and `$coords`     |
| `...`          | named args                              | Forwarded to `ggplot2::geom_point()`                |

**Returns:**
- `return_coords = FALSE`: a ggplot2 object
- `return_coords = TRUE`: list with `$plot` (ggplot2) and `$coords` (tibble with `dim1`, `dim2`, and optionally `label`)

**Implementation details:**
- Checks Suggests availability: ggplot2, and Rtsne or uwot depending on method
- t-SNE: `Rtsne::Rtsne(embeddings, dims = 2, perplexity = min(30, floor((nrow(embeddings) - 1) / 3)))` -- auto-adjusts perplexity for small datasets
- UMAP: `uwot::umap(embeddings, n_components = 2)`
- ggplot: `geom_point(aes(x = dim1, y = dim2, color = label), ...)` with `theme_minimal()`
- Axis labels: "t-SNE 1" / "t-SNE 2" or "UMAP 1" / "UMAP 2"
- When `labels` is NULL, no color aesthetic is mapped

### 3. Package Integration

**DESCRIPTION -- new Suggests:**
- `Rtsne (>= 0.17)`
- `uwot (>= 0.2.0)`
- (ggplot2 already in Suggests)

**NAMESPACE -- new exports:**
- `tabpfn_embeddings`
- `tabpfn_embeddings_cv`
- `extract_embeddings`
- `plot_embeddings`

**`_pkgdown.yml`:** Add embeddings article.

### 4. Vignette -- `vignettes/embeddings.Rmd`

Sections:
1. Vanilla embeddings on iris classifier (all three aggregate modes)
2. Vanilla embeddings on mtcars regressor
3. Cross-validated embeddings with `n_folds = 5`
4. Cross-validated embeddings with rsample folds
5. t-SNE and UMAP side-by-side plots
6. `return_coords = TRUE` for custom downstream use
7. Parsnip workflow extraction via `extract_embeddings()`

### 5. Tests -- `tests/testthat/test-embeddings.R`

- Vanilla embeddings: correct shape for each aggregate mode (mean, concat, none)
- Vanilla embeddings: works for both classifier and regressor
- CV embeddings: output covers all N samples in correct order
- CV embeddings: accepts rsample rset and list-of-indices
- `extract_embeddings()`: works on parsnip fit and workflow fit
- `plot_embeddings()`: returns ggplot object; with `return_coords = TRUE` returns list
- Input validation: wrong object type, 3D array passed to plot, mismatched labels length

## Out of Scope

- Fine-tuning embeddings
- Caching/serialization of embeddings
- Interactive plots (plotly, etc.)
- Custom dimensionality reduction methods beyond t-SNE and UMAP
