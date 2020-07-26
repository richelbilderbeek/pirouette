#' Convert a \code{pir_out} to its long form
#'
#' A \code{pir_out} is a table with
#' columns \code{tree} (for true or twin tree), a column
#' \code{inference_model} (for generative or candidate) and columns named
#' \code{error_1}, \code{error_2}, etcetera, containing the inference errors.
#'
#' Converting this to a long form, results in a tibble like this:
#'
#' \enumerate{
#'   \item tree_and_model: either \code{true_generative},
#'     or \code{twin_generative}, or \code{true_candidate},
#'     or \code{twin_candidate}
#'   \item error_value: inference errors
#' }
#' @inheritParams default_params_doc
#' @return the \code{pir_out} in long form
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @examples
#' pir_out <- create_test_pir_run_output(
#'   add_twin = TRUE,
#'   add_best = TRUE
#' )
#' convert_pir_out_to_long(pir_out)
#' @export
convert_pir_out_to_long <- function(
  pir_out,
  verbose = FALSE
) {
  tree <- NULL; rm(tree) # nolint, fixes warning: no visible binding for global variable

  pirouette::check_pir_out(pir_out)

  df <- tibble::as_tibble(pir_out)
  df <- dplyr::rename(df, tree_and_model = tree)
  df$tree_and_model <- interaction(
    df$tree_and_model,
    df$inference_model,
    sep = "_"
  )
  df$inference_model <- NULL
  df$inference_model_weight <- NULL
  df$site_model <- NULL
  df$clock_model <- NULL
  df$tree_prior <- NULL

  first_col_index <- which(names(df) == "error_1")
  testthat::expect_equal(1, length(first_col_index))

  df_long <- tidyr::gather(
    df, "error_index", "error_value", first_col_index:ncol(df)
  )
  testthat::expect_true("error_value" %in% names(df_long))

  # 'error_index' is added by gather, remove it
  testthat::expect_true("error_index" %in% names(df_long))
  df_long$error_index <- NULL

  if (isTRUE(verbose)) {
    message(utils::head(df_long))
  }
  df_long$tree_and_model <- forcats::as_factor(df_long$tree_and_model)

  df_long
}
