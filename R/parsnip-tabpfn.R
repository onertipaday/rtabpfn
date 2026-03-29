# Suppress R CMD check NOTE for NSE variables used inside rlang::expr() calls
# that R's static analysis cannot resolve.
utils::globalVariables(c("object", "new_data"))

#' TabPFN Model Specification for parsnip
#'
#' Creates a parsnip model specification for TabPFN.
#'
#' @param mode Character. Either `"classification"` or `"regression"`.
#' @param n_estimators Integer. Number of ensemble members. Default `8L`.
#' @param softmax_temperature Double. Temperature for softmax. Default `0.9`.
#' @param device Character. `"auto"`, `"cpu"`, or `"cuda"`. Default `"auto"`.
#' @param model_version Character or `NULL`. Default `NULL` (v2.6).
#'
#' @return A parsnip model specification.
#'
#' @export
tabpfn_model <- function(mode = "classification",
                          n_estimators = 8L,
                          softmax_temperature = 0.9,
                          device = "auto",
                          model_version = NULL) {
  if (!requireNamespace("parsnip", quietly = TRUE)) {
    rlang::abort("The parsnip package is required. Install with: install.packages('parsnip')")
  }

  valid_modes <- c("classification", "regression")
  if (!mode %in% valid_modes) {
    rlang::abort(paste0(
      'mode must be one of: ', paste0('"', valid_modes, '"', collapse = ", "),
      '. Got: "', mode, '"'
    ))
  }

  # Ensure parsnip registration has been done
  register_tabpfn_parsnip()

  parsnip::new_model_spec(
    "tabpfn_model",
    args = list(
      n_estimators        = rlang::enquo(n_estimators),
      softmax_temperature = rlang::enquo(softmax_temperature),
      device              = rlang::enquo(device),
      model_version       = rlang::enquo(model_version)
    ),
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = "tabpfn"
  )
}

#' Register TabPFN as a parsnip engine
#' @keywords internal
register_tabpfn_parsnip <- function() {
  # Guard: only register once
  env <- parsnip::get_model_env()
  if ("tabpfn_model" %in% env$models) {
    return(invisible(NULL))
  }

  parsnip::set_new_model("tabpfn_model")
  parsnip::set_model_mode("tabpfn_model", "classification")
  parsnip::set_model_mode("tabpfn_model", "regression")
  parsnip::set_model_engine("tabpfn_model", mode = "classification", eng = "tabpfn")
  parsnip::set_model_engine("tabpfn_model", mode = "regression", eng = "tabpfn")

  parsnip::set_dependency("tabpfn_model", eng = "tabpfn", pkg = "rtabpfn",
                          mode = "classification")
  parsnip::set_dependency("tabpfn_model", eng = "tabpfn", pkg = "rtabpfn",
                          mode = "regression")

  # Model args
  # func points to a dials parameter for tuning support; use dials::trees() for
  # n_estimators (semantically equivalent), and list() for params with no dials counterpart.
  parsnip::set_model_arg(
    model = "tabpfn_model", eng = "tabpfn",
    parsnip = "n_estimators", original = "n_estimators",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "tabpfn_model", eng = "tabpfn",
    parsnip = "softmax_temperature", original = "softmax_temperature",
    func = list(),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "tabpfn_model", eng = "tabpfn",
    parsnip = "device", original = "device",
    func = list(),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "tabpfn_model", eng = "tabpfn",
    parsnip = "model_version", original = "model_version",
    func = list(),
    has_submodel = FALSE
  )

  # Classification fit
  parsnip::set_fit(
    model = "tabpfn_model", mode = "classification", eng = "tabpfn",
    value = list(
      interface = "data.frame",
      protect   = c("x", "y"),
      func      = c(pkg = "rtabpfn", fun = "tabpfn_classifier"),
      defaults  = list()
    )
  )

  # Classification predict: class
  parsnip::set_pred(
    model = "tabpfn_model", mode = "classification", eng = "tabpfn",
    type = "class",
    value = list(
      pre  = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object   = rlang::expr(object$fit),
        new_data = rlang::expr(new_data),
        type     = "class"
      )
    )
  )

  # Classification predict: prob
  parsnip::set_pred(
    model = "tabpfn_model", mode = "classification", eng = "tabpfn",
    type = "prob",
    value = list(
      pre  = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object   = rlang::expr(object$fit),
        new_data = rlang::expr(new_data),
        type     = "prob"
      )
    )
  )

  # Regression fit
  parsnip::set_fit(
    model = "tabpfn_model", mode = "regression", eng = "tabpfn",
    value = list(
      interface = "data.frame",
      protect   = c("x", "y"),
      func      = c(pkg = "rtabpfn", fun = "tabpfn_regressor"),
      defaults  = list()
    )
  )

  # Regression predict: numeric
  parsnip::set_pred(
    model = "tabpfn_model", mode = "regression", eng = "tabpfn",
    type = "numeric",
    value = list(
      pre  = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object   = rlang::expr(object$fit),
        new_data = rlang::expr(new_data),
        type     = "numeric"
      )
    )
  )

  invisible(NULL)
}
