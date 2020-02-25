#' @export
check_gen_and_cand_exps_save_to_different_files <- function(experiments) { # nolint indeed a long function name
  if (length(experiments) < 2) return()
  if (experiments[[1]]$inference_conditions$model_type != "generative") {
    return()
  }
  testthat::expect_equal(
    experiments[[2]]$inference_conditions$model_type,
    "candidate"
  )
  if (experiments[[1]]$errors_filename == experiments[[2]]$errors_filename) {
    stop(
      "The errors filenames of generative and candidate experiments ",
      "must differ. ",
      "Actual value: ", experiments[[1]]$errors_filename
    )
  }
}
