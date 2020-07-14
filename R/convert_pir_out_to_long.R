#' Convert a \code{pir_out} to its long form
#' @inheritParams default_params_doc
#' @return the \code{pir_out} in long form
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @examples
#' library(testthat)
#'
#' pir_out <- create_test_pir_run_output()
#' t <- convert_pir_out_to_long(pir_out)
#' expect_equal(2, ncol(t))
#' expect_true("error_value" %in% names(t))
#' expect_true("tree_and_model" %in% names(t))
#' expect_silent(pir_plot_from_long(t))
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
