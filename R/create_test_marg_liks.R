#' Create a testing marginal likelihoods data frame.
#'
#' @inheritParams default_params_doc
#' @return a data frame
#' @examples
#' create_test_marg_liks()
#' @author Richèl J.C. Bilderbeek
#' @export
create_test_marg_liks <- function(
  site_models = beautier::create_site_models(),
  clock_models = beautier::create_clock_models(),
  tree_priors = beautier::create_tree_priors()
) {
  n_rows <- length(site_models) *
    length(clock_models) * length(tree_priors)

  site_model_names <- rep(NA, n_rows)
  clock_model_names <- rep(NA, n_rows)
  tree_prior_names <- rep(NA, n_rows)
  marg_log_liks <- rep(NA, n_rows)
  marg_log_lik_sds <- rep(NA, n_rows)

  # Pick a site model
  row_index <- 1
  for (site_model in site_models) {
    for (clock_model in clock_models) {
      for (tree_prior in tree_priors) {
        marg_log_liks[row_index] <- stats::runif(n = 1, min = -100, max = 0)
        marg_log_lik_sds[row_index] <- stats::runif(
          n = 1, min = -10000, max = -100
        )
        site_model_names[row_index] <- site_model$name
        clock_model_names[row_index] <- clock_model$name
        tree_prior_names[row_index] <- tree_prior$name
        row_index <- row_index + 1
      }
    }
  }

  weights <- as.numeric(
    mcbette::calc_weights(marg_liks = exp(Rmpfr::mpfr(marg_log_liks, 256)))
  )

  df <- data.frame(
    site_model_name = site_model_names,
    clock_model_name = clock_model_names,
    tree_prior_name = tree_prior_names,
    marg_log_lik = marg_log_liks,
    marg_log_lik_sd = marg_log_lik_sds,
    weight = weights,
    stringsAsFactors = FALSE
  )
  df
}
