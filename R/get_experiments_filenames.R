#' Extract the filesnames in the experiments
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @examples
#' get_experiments_filenames(
#'   experiments = list(create_test_experiment())
#' )
#' @export
get_experiments_filenames <- function(experiments) {
  check_experiments(experiments) # nolint pirouette function
  n_filenames_per_experiment <- length(
    get_experiment_filenames(experiments[[1]])
  )

  filenames <- rep(NA, n_filenames_per_experiment * length(experiments))
  for (i in seq_along(experiments)) {
    filenames_here <- get_experiment_filenames(experiments[[i]]) # nolint pirouette function
    i_minus_one <- i - 1
    from_index <- 1 + (i_minus_one * n_filenames_per_experiment)
    to_index <- from_index + n_filenames_per_experiment - 1
    filenames[from_index:to_index] <- filenames_here
  }
  filenames
}
