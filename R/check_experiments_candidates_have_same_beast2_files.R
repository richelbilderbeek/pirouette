#' Check if all experiments save to the same files
#'
#' Will \link{stop} if two experiments save to a different input,
#' trace, state, screen or tree file.
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @export
check_candidates_save_to_same_files <- function( # nolint long function
  experiments
) {
  testit::assert(length(experiments) >= 2)

  for (i in seq(1, length(experiments) - 1)) {
    testthat::expect_true(i >= 1)
    testthat::expect_true(i <= length(experiments))
    experiment_1 <- experiments[[i]]
    for (j in seq(i + 1, length(experiments))) {
      testthat::expect_true(j >= 1)
      testthat::expect_true(j > i)
      testthat::expect_true(j <= length(experiments))

      experiment_2 <- experiments[[j]]
      if (experiment_1$inference_conditions$model_type == "candidate" &&
          experiment_2$inference_conditions$model_type == "candidate"
      ) {
        input_filename_1 <- experiment_1$beast2_options$input_filename
        input_filename_2 <- experiment_2$beast2_options$input_filename
        state_filename_1 <- experiment_1$beast2_options$output_state_filename
        state_filename_2 <- experiment_2$beast2_options$output_state_filename

        # An unitialized log filename is NA
        log_filename_1 <- experiment_1$inference_model$mcmc$tracelog$filename
        log_filename_2 <- experiment_2$inference_model$mcmc$tracelog$filename
        screen_filename_1 <- experiment_1$inference_model$mcmc$screenlog$filename # nolint long name, sorry Demeter
        screen_filename_2 <- experiment_2$inference_model$mcmc$screenlog$filename # nolint long name, sorry Demeter
        trees_filename_1 <- experiment_1$inference_model$mcmc$treelog$filename
        trees_filename_2 <- experiment_2$inference_model$mcmc$treelog$filename
        if (input_filename_1 != input_filename_2) {
          stop(
            "Candidate models must have same BEAST2 input filename. \n",
            "This is to assure they work on the same input. \n",
            "Difference between experiments #", i, " and #", j, ". \n",
            "Filename #", i, ": ", input_filename_1, "\n",
            "Filename #", j, ": ", input_filename_2, "\n"
          )
        }
        # OK: both NA or both the same string
        # Check if only 1 NA
        if (sum(is.na(c(log_filename_1, log_filename_2))) == 1) {
          stop(
            "Candidate models must have same MCMC tracelog filename. \n",
            "Difference between experiments #", i, " and #", j, ". \n",
            "Filename #", i, ": ", log_filename_1, "\n",
            "Filename #", j, ": ", log_filename_2, "\n"
          )
        }
        # Check for two strings
        if (!beautier::is_one_na(log_filename_1) &&
            log_filename_1 != log_filename_2) {
          stop(
            "Candidate models must have same MCMC tracelog filename. \n",
            "Difference between experiments #", i, " and #", j, ". \n",
            "Filename #", i, ": ", log_filename_1, "\n",
            "Filename #", j, ": ", log_filename_2, "\n"
          )
        }
        if (screen_filename_1 != screen_filename_2) {
          stop(
            "Candidate models must have same MCMC screenlog filename. \n",
            "Difference between experiments #", i, " and #", j, ". \n",
            "Filename #", i, ": ", screen_filename_1, "\n",
            "Filename #", j, ": ", screen_filename_2, "\n"
          )
        }
        if (trees_filename_1 != trees_filename_2) {
          stop(
            "Candidate models must have same MCMC treelog filename. \n",
            "Difference between experiments #", i, " and #", j, ". \n",
            "Filename #", i, ": ", trees_filename_1, "\n",
            "Filename #", j, ": ", trees_filename_2, "\n"
          )
        }
        if (state_filename_1 != state_filename_2) {
          stop(
            "Candidate models must have same BEAST2 output state filename. \n",
            "Difference between experiments #", i, " and #", j, ". \n",
            "Filename #", i, ": ", state_filename_1, "\n",
            "Filename #", j, ": ", state_filename_2, "\n"
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
}
