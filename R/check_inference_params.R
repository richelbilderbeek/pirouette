#' Check if the inference parameters are valid.
#' Throws if these are invalid
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
check_inference_params <- function(
  inference_params
) {
  argument_names <- c(
    "site_model", "clock_model", "tree_prior", "mrca_prior",
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
  beautier::check_site_model(inference_params$site_model)
  beautier::check_clock_model(inference_params$clock_model)
  beautier::check_tree_prior(inference_params$tree_prior)

  rng_seed <- inference_params$rng_seed
  if (!beautier:::is_one_na(rng_seed) && rng_seed <= 0) {
    stop("'rng_seed' should be NA or non-zero positive")
  }
}
