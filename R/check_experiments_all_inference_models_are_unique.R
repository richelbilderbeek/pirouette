#' Check if all experiments have unique inference models.
#'
#' Will \link{stop} if two models have a same site and clock and tree prior.
#' Note that experiments that differ in their MRCA priors only
#' are classified being the same.
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @export
check_experiments_all_inference_models_are_unique <- function( # nolint indeed a long function, which is fine for an internal function
  experiments
) {
  testthat::expect_true(length(experiments) >= 2)
  for (i in seq(1, length(experiments) - 1)) {

    site_model_name_1 <- experiments[[i]]$inference_model$site_model$name
    clock_model_name_1 <- experiments[[i]]$inference_model$clock_model$name
    tree_prior_name_1 <- experiments[[i]]$inference_model$tree_prior$name
    for (j in seq(i + 1, length(experiments))) {
      testthat::expect_true(j > i)
      testthat::expect_true(j <= length(experiments))
      site_model_name_2 <- experiments[[j]]$inference_model$site_model$name
      clock_model_name_2 <- experiments[[j]]$inference_model$clock_model$name
      tree_prior_name_2 <- experiments[[j]]$inference_model$tree_prior$name
      if (site_model_name_1  == site_model_name_2 &&
          clock_model_name_1 == clock_model_name_2 &&
          tree_prior_name_1  == tree_prior_name_2
      ) {
        stop(
          "All inference models must be unique\n",
          "Duplicate between experiment[[", i, "]] ",
          "and experiment[[", j, "]].\n"
        )
      }
    }
  }
}
