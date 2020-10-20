#' Plot the output of \link{pir_runs}.
#' @inheritParams default_params_doc
#' @return a \code{ggplot2} plot
#' @author Richèl J.C. Bilderbeek
#' @examples
#'
#' # Create fake pir_run output
#' pir_outs <- list()
#' pir_outs[[1]] <- create_test_pir_run_output(
#'   add_twin = TRUE,
#'   add_best = TRUE
#' )
#' pir_outs[[2]] <- pir_outs[[1]]
#'
#' # Plot the (fake) output
#' pir_plots(pir_outs)
#' @export
pir_plots <- function( # nolint indeed high cyclomatic complexity, will fix after getting correct code
  pir_outs,
  check_input = TRUE,
  verbose = FALSE
) {
  # Check input
  if (isTRUE(check_input)) {
    if (isTRUE(verbose)) message("Check inputs: ", Sys.time())
    testit::assert(length(pir_outs) >= 1)
    for (pir_out in pir_outs) {
      if (isTRUE(verbose)) message("Check input: ", Sys.time())
      pirouette::check_pir_out(pir_out)
    }
  }

  # All elements must have the same number of rows and columns
  if (isTRUE(verbose)) {
    message(
      "All elements must have the same number of rows and columns: ",
      Sys.time()
    )
  }
  pir_out <- pir_outs[[1]]
  n_rows <- nrow(pir_out)
  n_cols <- ncol(pir_out)
  for (pir_out in pir_outs) {
    testthat::expect_equal(n_rows, nrow(pir_out))
    testthat::expect_equal(n_cols, ncol(pir_out))
  }

  # Count the number of errors
  if (isTRUE(verbose)) message("Count the number of errors: ", Sys.time())
  first_col_index <- which(names(pir_out) == "error_1")
  n_errors <- n_cols - first_col_index + 1

  # Resize pir_out to be able to hold the errors of all data frames
  if (isTRUE(verbose)) {
    message(
      "Resize pir_out to be able to hold the errors of all data frames: ",
      Sys.time()
    )
  }
  n_all_errors <- n_errors * length(pir_outs)
  pir_out[, paste0("error_", seq(1, n_all_errors))] <- NA

  # Copy the errors
  if (isTRUE(verbose)) message("Copy the errors: ", Sys.time())
  for (i in seq_along(pir_outs)) {
    if (isTRUE(verbose)) message("Copy the errors ", i, ": ", Sys.time())
    from_begin_col <- first_col_index
    from_end_col <- n_cols
    to_begin_col <- from_begin_col + ((i - 1) * n_errors)
    to_end_col <- n_cols + ((i - 1) * n_errors)
    pir_out[, to_begin_col:to_end_col] <-
      pir_outs[[i]][, from_begin_col:from_end_col]
  }

  # Plot it
  if (isTRUE(verbose)) message("Plot it: ", Sys.time())
  pirouette::pir_plot(pir_out)
}
