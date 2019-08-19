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
check_experiments <- function(
  experiments
) {
  if (!is.list(experiments)) {
    stop(
      "'experiments' must be a list of one or more experiments.\n",
      "Actual value: ", experiments
    )
  }
  for (i in seq_along(experiments)) {
    tryCatch(
      check_experiment(experiments[[i]]), # nolint pirouette function
      error = function(e) {
        stop(
          "'experiments[[", i, "]] invalid.\n",
          "Tip: use 'create_experiment' for each list element.\n",
          "Error: ", e$message, "\n",
          "Value: ", experiments[[i]]
        )
      }
    )
  }
  if (length(experiments) == 1) return()

  testit::assert(length(experiments) >= 2)

  check_experiments_candidates_have_same_beast2_files(experiments)
  check_experiments_candidates_have_same_mcmcs(experiments)

  model_types <- rep("", length(experiments))
  for (i in 1:length(experiments)) {
    model_types[i] <- experiments[[i]]$inference_conditions$model_type
  }
  if (sum(model_types == "generative") > 1) {
    stop("Specifying more than one 'generative' model experiment is redundant")
  }
  testit::assert(length(experiments) >= 2)
  exp_types <- rep(NA, length(experiments))
  for (i in seq_along(experiments)) {
    exp_types[i] <- experiments[[i]]$inference_conditions$model_type
  }
  if (exp_types[1] != "generative" && ("generative" %in% exp_types)) {
    stop("If multiple experiments, generative is either first or absent")
  }
  check_experiments_all_inference_models_are_unique(experiments)
}
