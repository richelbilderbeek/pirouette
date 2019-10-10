#' @author Richèl J.C. Bilderbeek
#' @export
check_experiments_candidates_have_same_beast2_files <- function( # nolint long function
  experiments
) {
  testit::assert(length(experiments) >= 2)

  for (i in seq(1, length(experiments) - 1)) {
    experiment_1 <- experiments[[i]]
    for (j in seq(i + 1, length(experiments))) {
      testit::assert(j > i)
      experiment_2 <- experiments[[j]]
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
}
