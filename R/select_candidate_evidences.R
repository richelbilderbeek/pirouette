#' Select the evidences for candidate experiments
#'
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @examples
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
    if (experiment$inference_conditions$model_type == "generative") next () # nolint @lintr-bot likes a space between next and the braces?
    for (i in seq(1, nrow(marg_liks))) {
      # Marginal Likelihood Site Model Name
      marg_liks_names <- c(
        as.character(marg_liks$site_model_name[i]),
        as.character(marg_liks$clock_model_name[i]),
        as.character(marg_liks$tree_prior_name[i])
      )
      experiment_names <- c(
        experiment$inference_model$site_model$name,
        experiment$inference_model$clock_model$name,
        experiment$inference_model$tree_prior$name
      )
      # Should be the same names for the same site/clock/tree models
      testit::assert(marg_liks_names[1] %in% beautier::get_site_model_names())
      testit::assert(marg_liks_names[2] %in% beautier::get_clock_model_names())
      testit::assert(marg_liks_names[3] %in% beautier::get_tree_prior_names())
      testit::assert(experiment_names[1] %in% beautier::get_site_model_names())
      testit::assert(experiment_names[2] %in% beautier::get_clock_model_names())
      testit::assert(experiment_names[3] %in% beautier::get_tree_prior_names())

      if (all(identical(marg_liks_names, experiment_names))) {
        selected_row_indices <- c(selected_row_indices, i)
      }
    }
  }

  marg_liks[selected_row_indices, ]
}
