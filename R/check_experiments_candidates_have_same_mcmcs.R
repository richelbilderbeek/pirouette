#' Check that the candidate experiments have a same MCMC
#'
#' If all the candidates share a same MCMC, they share the same
#' tracelog and treelog. In this way, one can predict where the parameter
#' estimates (the trace) and posterior trees are written to, as only the
#' best candidate will run.
#' @inheritParams default_params_doc
#' @return nothing
#' @author Richèl J.C. Bilderbeek
#' @export
check_experiments_candidates_have_same_mcmcs <- function( # nolint indeed a long name
  experiments
) {
  testthat::expect_true(length(experiments) >= 2)
  for (i in seq(1, length(experiments) - 1)) {
    experiment_1 <- experiments[[i]]
    if (experiment_1$inference_conditions$model_type != "candidate") next
    for (j in seq(i + 1, length(experiments))) {
      testthat::expect_true(j > i)
      experiment_2 <- experiments[[j]]
      if (experiment_2$inference_conditions$model_type != "candidate") next
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
  invisible(experiments)
}
