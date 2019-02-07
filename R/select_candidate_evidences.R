#' Select the evidences for candidate experiments
#'
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @export
select_candidate_evidences <- function(
  experiments = list(create_test_experiment()),
  marg_liks = create_test_marg_liks()
) {
  check_experiments(experiments) # nolint pirouette function

  selected_row_indices <- c()

  for (experiment in experiments) {
    if (experiment$model_type == "generative") next()
    for (i in seq(1: nrow(marg_liks))) {
      if (marg_liks$site_model_name[i] == experiment$inference_model$site_model$name &&
        marg_liks$clock_model_name[i] == experiment$inference_model$clock_model$name &&
        marg_liks$tree_prior_name[i] == experiment$inference_model$tree_prior$name
      ) {
        selected_row_indices <- c(selected_row_indices, i)
      }
    }
  }

  marg_liks[selected_row_indices, ]
}
