#' @export
#' @inheritParams default_params_doc
get_experiments_filenames <- function(experiments) {
  check_experiments(experiments)
  n_filenames_per_experiment <- length(
    get_experiment_filenames(experiments[[1]])
  )

  filenames <- rep(NA, n_filenames_per_experiment * length(experiments))
  for (i in seq_along(experiments)) {
    filenames_here <- get_experiment_filenames(experiments[[i]])

    from_index <- 1 + ((i - 1) * n_filenames_per_experiment)
    to_index <- from_index + n_filenames_per_experiment - 1
    filenames[from_index:to_index] <- filenames_here
  }
  filenames
}
