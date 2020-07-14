#' Plot the error 'BEAST2' makes from a known phylogeny
#' from tidy data
#' @inheritParams default_params_doc
#' @return a \code{ggplot2} plot
#' @author Richèl J.C. Bilderbeek
#' @export
create_long_pir_out_from_folder <- function(# nolint indeed a long function name
  folder_name
) {
  errors_filenames <- c(
    list.files(path = folder_name, pattern = "^gen_errors.csv$", full.names = TRUE), # nolint indeed a long line
    list.files(path = folder_name, pattern = "^gen_errors_twin.csv$", full.names = TRUE), # nolint indeed a long line
    list.files(path = folder_name, pattern = "^best_errors.csv$", full.names = TRUE), # nolint indeed a long line
    list.files(path = folder_name, pattern = "^best_errors_twin.csv$", full.names = TRUE) # nolint indeed a long line
  )
  testthat::expect_equal(4, length(errors_filenames))
  testthat::expect_true(all(file.exists(errors_filenames)))

  # Filenames and 'tree_and_model' values must match
  testthat::expect_equal(get_tree_and_model_values()[1], "true_generative")
  testthat::expect_match(errors_filenames[1], "gen_errors")
  testthat::expect_equal(get_tree_and_model_values()[2], "twin_generative")
  testthat::expect_match(errors_filenames[2], "gen_errors_twin")
  testthat::expect_equal(get_tree_and_model_values()[3], "true_candidate")
  testthat::expect_match(errors_filenames[3], "best_errors")
  testthat::expect_equal(get_tree_and_model_values()[4], "twin_candidate")
  testthat::expect_match(errors_filenames[4], "best_errors_twin")

  n_errors <- length(pirouette::read_errors_csv(errors_filenames[1]))

  t <- tibble::tibble(
    tree_and_model = rep(get_tree_and_model_values(), each = n_errors),
    error_value = c(
      pirouette::read_errors_csv(errors_filenames[1]),
      pirouette::read_errors_csv(errors_filenames[2]),
      pirouette::read_errors_csv(errors_filenames[3]),
      pirouette::read_errors_csv(errors_filenames[4])
    )
  )
  t$tree_and_model <- forcats::as_factor(t$tree_and_model)
  t
}
