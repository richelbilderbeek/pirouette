#' @author Richèl J.C. Bilderbeek
#' @noRd
check_experiments_candidates_have_same_mcmcs <- function(
  experiments
) {
  testit::assert(length(experiments) >= 2)
  for (i in seq(1, length(experiments) - 1)) {
    experiment_1 <- experiments[[i]]
    for (j in seq(i + 1, length(experiments))) {
      testit::assert(j > i)
      experiment_2 <- experiments[[j]]
      if (
        !beautier::are_equal_mcmcs(
          experiment_1$inference_model$mcmc,
          experiment_2$inference_model$mcmc
        )
      ) {
        stop(
          "All MCMCs in the experiments must be identical.\n",
          "Difference between experiment[[", i, "]] ",
          "and experiment[[", j, "]].\n",
          "Value experiment[[", i, "]]$inference_model$mcmc: ",
          paste0(experiments[[i]]$inference_model$mcmc, collapse = ", "), "\n",
          "Value experiment[[", j, "]]$inference_model$mcmc: ",
          paste0(experiments[[j]]$inference_model$mcmc, collapse = ", "), "\n"
        )
      }
    }
  }
}
