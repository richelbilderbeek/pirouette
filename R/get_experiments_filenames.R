#' Extract the filesnames in the experiments
#' @inheritParams default_params_doc
#' @return a character vector
#' @author Richèl J.C. Bilderbeek
#' @examples
#' if (beautier::is_on_ci()) {
#'   get_experiments_filenames(
#'     experiments = list(create_test_experiment())
#'   )
#' }
#' @export
get_experiments_filenames <- function(experiments) {
  pirouette::check_experiments(experiments)
  n_filenames_per_experiment <- length(
    pirouette::get_experiment_filenames(experiments[[1]])
  )

  filenames <- rep(NA, n_filenames_per_experiment * length(experiments))
  for (i in seq_along(experiments)) {
    filenames_here <- pirouette::get_experiment_filenames(experiments[[i]])
    i_minus_one <- i - 1
    from_index <- 1 + (i_minus_one * n_filenames_per_experiment)
    to_index <- from_index + n_filenames_per_experiment - 1
    filenames[from_index:to_index] <- filenames_here
  }
  filenames <- filenames[filenames != ""]
  testthat::expect_true(length(filenames[filenames == ""]) == 0)
  filenames
}
