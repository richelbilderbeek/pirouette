#' Check if the inference parameters are valid.
#' Throws if these are invalid
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
check_inference_params <- function(
  inference_params
) {
  argument_names <- c(
    "model_selection", "site_model", "clock_model", "tree_prior", "mrca_prior",
    "mcmc", "rng_seed", "beast2_path", "verbose"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(inference_params)) {
      stop(
        "'", arg_name, "' must be an element of an 'inference_params'. ",
        "Tip: use 'create_inference_params'"
      )
    }
  }
  if (!all(inference_params$model_selection %in% get_model_selections())) {
    stop("All values of 'model_selection' must be in 'get_model_selections()'")
  }
  rng_seed <- inference_params$rng_seed
  if (!beautier:::is_one_na(rng_seed) && rng_seed <= 0) {
    stop("'rng_seed' should be NA or non-zero positive")
  }
}
