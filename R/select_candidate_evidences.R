#' Select the evidences for candidate experiments
#'
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' if (rappdirs::app_dir()$os != "win") {
#'   experiment_1 <- create_test_gen_experiment()
#'   experiment_2 <- create_test_cand_experiment()
#'   expect_equal(experiment_1$inference_conditions$model_type, "generative")
#'   expect_equal(experiment_2$inference_conditions$model_type, "candidate")
#'   experiments <- list(experiment_1, experiment_2)
#'
#'   # Experiments must have different inference models
#'   experiments[[1]]$inference_model$site_model <- create_gtr_site_model()
#'
#'   candidate_evidences <- select_candidate_evidences(
#'     experiments = experiments,
#'     marg_liks = create_test_marg_liks()
#'   )
#'   expect_equal(1, nrow(candidate_evidences))
#' }
#' @export
select_candidate_evidences <- function(
  experiments = list(create_test_experiment()),
  marg_liks = create_test_marg_liks()
) {
  pirouette::check_experiments(experiments)

  selected_row_indices <- c()

  for (experiment in experiments) {
    if (experiment$inference_conditions$model_type == "generative") next()
    for (i in seq(1, nrow(marg_liks))) {
      if (marg_liks$site_model_name[i] ==
          experiment$inference_model$site_model$name &&
        marg_liks$clock_model_name[i] ==
          experiment$inference_model$clock_model$name &&
        marg_liks$tree_prior_name[i] ==
          experiment$inference_model$tree_prior$name
      ) {
        selected_row_indices <- c(selected_row_indices, i)
      }
    }
  }

  marg_liks[selected_row_indices, ]
}
