#' Validate classifier inputs
#'
#' @param x A data.frame, tibble, data.table, or matrix.
#' @param y A factor, character, or integer vector.
#' @return A factor vector (validated and possibly coerced).
#' @keywords internal
validate_classifier_inputs <- function(x, y) {
  if (nrow(x) != length(y)) {
    rlang::abort("x and y must have the same number of observations.")
  }
  if (is.factor(y)) {
    return(y)
  }
  if (is.character(y)) {
    return(as.factor(y))
  }
  if (is.integer(y)) {
    return(as.factor(y))
  }
  rlang::abort(
    "y must be a factor, character, or integer vector for classification.",
    class = "rtabpfn_input_error"
  )
}

#' Validate regressor inputs
#'
#' @param x A data.frame, tibble, data.table, or matrix.
#' @param y A numeric vector.
#' @return A double vector (validated).
#' @keywords internal
validate_regressor_inputs <- function(x, y) {
  if (nrow(x) != length(y)) {
    rlang::abort("x and y must have the same number of observations.")
  }
  if (!is.numeric(y)) {
    rlang::abort(
      "y must be numeric for regression.",
      class = "rtabpfn_input_error"
    )
  }
  as.double(y)
}

# Package-level constants
.tabpfn_row_limit <- 50000L
.tabpfn_col_limit <- 2000L
.tabpfn_class_limit <- 10L
.tabpfn_default_version <- "v2.6"

#' Select feature columns matching those used during training
#'
#' @param new_data A data.frame or matrix of new observations.
#' @param feature_names Character vector of feature names from training.
#' @return `new_data`, possibly subsetted to feature columns only.
#' @keywords internal
select_training_features <- function(new_data, feature_names) {
  if (is.data.frame(new_data) && !is.null(feature_names)) {
    matching <- intersect(feature_names, colnames(new_data))
    if (length(matching) == length(feature_names)) {
      new_data <- new_data[, feature_names, drop = FALSE]
    }
  }
  new_data
}

#' Check data constraints for TabPFN
#'
#' Validates that training data is within the limits supported by TabPFN.
#'
#' @param x A data.frame, tibble, data.table, or matrix of predictors.
#' @param y A vector of outcomes (factor for classification, numeric for regression).
#' @param ignore_limits Logical. If `TRUE`, skip row/column checks (class limit
#'   is always enforced). Default `FALSE`.
#' @return Invisible `NULL`. Called for side effects (errors on constraint violation).
#' @keywords internal
check_data_constraints <- function(x, y, ignore_limits = FALSE) {
  n_rows <- nrow(x)
  n_cols <- ncol(x)

  if (!ignore_limits && n_rows > .tabpfn_row_limit) {
    rlang::abort(
      c(
        paste0("Training set has ", format(n_rows, big.mark = ","),
               " rows, but TabPFN supports <= ",
               format(.tabpfn_row_limit, big.mark = ","), "."),
        i = "Use `training_set_limit` to automatically subsample.",
        i = "Or set `ignore_pretraining_limits = TRUE` to bypass this check."
      ),
      class = "rtabpfn_data_constraint_error"
    )
  }

  if (!ignore_limits && n_cols > .tabpfn_col_limit) {
    rlang::abort(
      c(
        paste0("Training set has ", format(n_cols, big.mark = ","),
               " predictors, but TabPFN supports <= ",
               format(.tabpfn_col_limit, big.mark = ","), "."),
        i = "Reduce the number of predictors before fitting.",
        i = "Or set `ignore_pretraining_limits = TRUE` to bypass this check."
      ),
      class = "rtabpfn_data_constraint_error"
    )
  }

  if (is.factor(y) || is.character(y)) {
    n_classes <- length(unique(y))
    if (n_classes > .tabpfn_class_limit) {
      rlang::abort(
        c(
          paste0("Outcome has ", n_classes, " classes, but TabPFN supports <= ",
                 .tabpfn_class_limit, "."),
          x = "This limit cannot be bypassed."
        ),
        class = "rtabpfn_data_constraint_error"
      )
    }
  }

  invisible(NULL)
}

#' Subsample training data with stratification
#'
#' For classification, stratifies by class. For regression, stratifies by
#' outcome quartile. Returns indices to keep.
#'
#' @param x A data.frame, tibble, data.table, or matrix of predictors.
#' @param y A vector of outcomes.
#' @param limit Integer. Maximum number of training samples.
#' @return An integer vector of row indices to keep, or `NULL` if no
#'   subsampling is needed.
#' @keywords internal
subsample_training_set <- function(x, y, limit) {
  n <- nrow(x)
  if (n <= limit) {
    return(NULL)
  }

  if (is.factor(y) || is.character(y)) {
    # Stratify by class
    groups <- as.character(y)
  } else {
    # Stratify by quartile for regression
    groups <- as.character(dplyr::ntile(y, n = 4L))
  }

  group_tab <- table(groups)
  group_props <- group_tab / n

  # Proportional allocation with remainder redistribution
  group_alloc <- floor(group_props * limit)
  remainder <- limit - sum(group_alloc)
  if (remainder > 0) {
    # Distribute remaining slots to groups with largest fractional parts
    frac <- (group_props * limit) - group_alloc
    top_groups <- names(sort(frac, decreasing = TRUE))[seq_len(remainder)]
    group_alloc[top_groups] <- group_alloc[top_groups] + 1L
  }

  idx_list <- vector("list", length(group_tab))
  for (i in seq_along(group_tab)) {
    g <- names(group_tab)[[i]]
    g_idx <- which(groups == g)
    n_take <- min(group_alloc[[g]], length(g_idx))
    idx_list[[i]] <- sample(g_idx, n_take)
  }

  sort(unlist(idx_list, use.names = FALSE))
}

#' Map R model version string to Python ModelVersion enum
#'
#' @param version A string: `"v2"`, `"v2.5"`, or `"v2.6"`, or `NULL`.
#' @return A Python `ModelVersion` enum value, or `NULL` if `version` is `NULL`.
#' @keywords internal
resolve_model_version <- function(version) {
  if (is.null(version)) {
    return(NULL)
  }
  valid <- c("v2", "v2.5", "v2.6")
  if (!version %in% valid) {
    rlang::abort(
      paste0(
        "model_version must be one of: ",
        paste0('"', valid, '"', collapse = ", "),
        ". Got: \"", version, '"'
      ),
      class = "rtabpfn_input_error"
    )
  }
  mv <- tabpfn_constants$ModelVersion
  switch(version,
    "v2"   = mv$V2,
    "v2.5" = mv$V2_5,
    "v2.6" = mv$V2_6
  )
}
