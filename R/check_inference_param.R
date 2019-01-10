#' Check if the inference parameters are valid.
#' Throws if these are invalid
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
check_inference_param <- function(
  inference_param
) {
  argument_names <- c(
    # "site_model", "clock_model", "tree_prior",
    "mrca_prior", "mcmc", "rng_seed", "beast2_path", "verbose"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(inference_param)) {
      stop(
        "'", arg_name, "' must be an element of an 'inference_param'. ",
        "Tip: use 'create_inference_param'"
      )
    }
  }
  #beautier::check_site_model(inference_param$site_model)
  #beautier::check_clock_model(inference_param$clock_model)
  #beautier::check_tree_prior(inference_param$tree_prior)

  rng_seed <- inference_param$rng_seed
  if (!beautier:::is_one_na(rng_seed) && rng_seed <= 0) {
    stop("'rng_seed' should be NA or non-zero positive")
  }
}
