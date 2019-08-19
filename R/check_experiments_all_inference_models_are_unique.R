#' Checks if the argument is a list of one or more \link{pirouette} experiments.
#'
#' Will \link{stop} if not.
#' A valid \link{pirouette} experiment
#' can be created by \link{create_experiment}.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @seealso Use \link{check_experiment} to check if an object
#'   is one valid experiment
#' @examples
#' library(testthat)
#'
#' expect_silent(check_experiments(list(create_test_experiment())))
#' expect_error(check_experiments(create_test_experiment()))
#' expect_error(check_experiments("nonsense"))
#' expect_error(check_experiments(NA))
#' expect_error(check_experiments(NULL))
#'
#' if (rappdirs::app_dir()$os != "win") {
#'   expect_silent(
#'     check_experiments(
#'       list(
#'         create_test_experiment(),
#'         create_test_cand_experiment()
#'       )
#'     )
#'   )
#' }
#' @author Richèl J.C. Bilderbeek
#' @export
check_experiments_all_inference_models_are_unique <- function(
  experiments
) {
  testit::assert(length(experiments) >= 2)
  for (i in seq(1, length(experiments) - 1)) {

    site_model_name_1 <- experiments[[i]]$inference_model$site_model$name
    clock_model_name_1 <- experiments[[i]]$inference_model$clock_model$name
    tree_prior_name_1 <- experiments[[i]]$inference_model$tree_prior$name
    for (j in seq(i + 1, length(experiments))) {
      testit::assert(j > i)
      testit::assert(j <= length(experiments))
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
