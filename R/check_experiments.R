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
      if (experiment_1$inference_conditions$model_type == "candidate" &&
          experiment_2$inference_conditions$model_type == "candidate"
      ) {
        input_filename_1 <- experiment_1$beast2_options$input_filename
        input_filename_2 <- experiment_2$beast2_options$input_filename
        output_log_filename_1 <- experiment_1$beast2_options$output_log_filename
        output_log_filename_2 <- experiment_2$beast2_options$output_log_filename
        output_trees_filenames_1 <- experiment_1$beast2_options$output_trees_filenames # nolint long names indeed, sorry Demeter
        output_trees_filenames_2 <- experiment_2$beast2_options$output_trees_filenames # nolint long names indeed, sorry Demeter
        output_state_filename_1 <- experiment_1$beast2_options$output_state_filename # nolint long names indeed, sorry Demeter
        output_state_filename_2 <- experiment_2$beast2_options$output_state_filename # nolint long names indeed, sorry Demeter
        if (input_filename_1 != input_filename_2) {
          stop(
            "Candidate models must have same BEAST2 input filename. \n",
            "Difference between experiments #", i, " and #", j, ". \n",
            "Filename #", i, ": ", input_filename_1, "\n",
            "Filename #", j, ": ", input_filename_2, "\n"
          )
        }
        if (output_log_filename_1 != output_log_filename_2) {
          stop(
            "Candidate models must have same BEAST2 output log filename. \n",
            "Difference between experiments #", i, " and #", j, ". \n",
            "Filename #", i, ": ", output_log_filename_1, "\n",
            "Filename #", j, ": ", output_log_filename_2, "\n"
          )
        }
        if (output_trees_filenames_1 != output_trees_filenames_2) {
          stop(
            "Candidate models must have same BEAST2 output trees filename. \n",
            "Difference between experiments #", i, " and #", j, ". \n",
            "Filename #", i, ": ", output_trees_filenames_1, "\n",
            "Filename #", j, ": ", output_trees_filenames_2, "\n"
          )
        }
        if (output_state_filename_1 != output_state_filename_2) {
          stop(
            "Candidate models must have same BEAST2 output state filename. \n",
            "Difference between experiments #", i, " and #", j, ". \n",
            "Filename #", i, ": ", output_state_filename_1, "\n",
            "Filename #", j, ": ", output_state_filename_2, "\n"
          )
        }
        if (experiment_1$errors_filename != experiment_2$errors_filename) {
          stop(
            "Candidate models must have same errors filename.\n",
            "Difference between experiments #", i, " and #", j, ". \n",
            "Filenames #", i, ": ", experiment_1$errors_filename, "\n",
            "Filenames #", j, ": ", experiment_2$errors_filename, "\n"
          )
        }
      }
    }
  }

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
  if (exp_types[1] == "generative") {
    gen_inference_model <- experiments[[1]]$inference_model
    gen_site_model_name <- gen_inference_model$site_model$name
    gen_clock_model_name <- gen_inference_model$clock_model$name
    gen_tree_prior_name <- gen_inference_model$tree_prior$name
    for (experiment in experiments[c(-1)]) {
      testit::assert(
        experiment$inference_conditions$model_type == "candidate"
      )
      cand_inference_model <- experiment$inference_model
      cand_site_model_name <- cand_inference_model$site_model$name
      cand_clock_model_name <- cand_inference_model$clock_model$name
      cand_tree_prior_name <- cand_inference_model$tree_prior$name
      if (gen_site_model_name == cand_site_model_name &&
        gen_clock_model_name == cand_clock_model_name &&
        gen_tree_prior_name == cand_tree_prior_name
      ) {
        stop(
          "Generative and candidate model cannot have the same inference model"
        )
      }

    }
  }
}
