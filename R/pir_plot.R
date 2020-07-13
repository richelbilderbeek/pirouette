#' Plot the error 'BEAST2' makes from a known phylogeny
#' @param pir_out the output created by \code{\link{pir_run}}
#' @return a \code{ggplot2} plot
#' @seealso
#' \itemize{
#'   \item Use \link{create_test_pir_run_output} to create a test output
#'     of \link{pir_run}.
#'   \item Use \link{pir_plot_from_file} to plot the errors after have
#'     being saved to a \code{.csv} file
#'   \item Use \link{pir_plots} to plot the output of multiple runs,
#'     for example, the output of \link{pir_runs}
#' }
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @examples
#' pir_out <- create_test_pir_run_output(
#'   add_twin = TRUE,
#'   add_best = TRUE
#' )
#' pir_plot(pir_out)
#' @export
pir_plot <- function(pir_out) {
  pirouette::check_pir_out(pir_out)

  # Satisfy R CMD check
  tree <- NULL; rm(tree) # nolint, fixes warning: no visible binding for global variable
  error_value <- NULL; rm(error_value) # nolint, fixes warning: no visible binding for global variable
  inference_model <- NULL; rm(inference_model) # nolint, fixes warning: no visible binding for global variable
  quantile <- NULL; rm(quantile) # nolint, fixes warning: no visible binding for global variable
  ..y.. <- NULL; rm(..y..) # nolint, fixes warning: no visible binding for global variable
  model_setting <- NULL; rm(model_setting) # nolint, fixes warning: no visible binding for global variable
  tree_and_model <- NULL; rm(tree_and_model) # nolint, fixes warning: no visible binding for global variable
  median <- NULL; rm(median) # nolint, fixes warning: no visible binding for global variable
  ..density.. <- NULL; rm(..density..) # nolint, fixes warning: no visible binding for global variable

  ##### Data wrangling #####
  # Convert to long form
  df <- pir_out
  first_col_index <- which(names(df) == "error_1")
  testthat::expect_equal(1, length(first_col_index))

  df_long <- tidyr::gather(
    df, "error_index", "error_value", first_col_index:ncol(df)
  )
  testthat::expect_true("error_index" %in% names(df_long))
  testthat::expect_true("error_value" %in% names(df_long))

  # Convert factor values to human-readable strings
  df_long$site_model <- plyr::revalue(
    df_long$site_model, c("JC69" = "JC", "TN93" = "TN"), warn_missing = FALSE)
  df_long$clock_model <- plyr::revalue(
    df_long$clock_model,
    c("strict" = "Strict", "relaxed_log_normal" = "RLN"), warn_missing = FALSE
  )
  df_long$tree_prior <- plyr::revalue(
    df_long$tree_prior,
    c(
      "yule" = "Yule",
      "birth_death" = "BD",
      "coalescent_bayesian_skyline" = "CBS",
      "coalescent_constant_population" = "CCP",
      "coalescent_exp_population" = "CEP"
    ),
    warn_missing = FALSE
  )

  # Add column tree_and_model, the combination of tree and model type
  df_long$tree_and_model <- interaction(
    df_long$tree,
    df_long$inference_model,
    sep = "_"
  )

  # Add model_setting, the combination of all inference models
  df_long$model_setting <- interaction(
    df_long$site_model,
    df_long$clock_model,
    df_long$tree_prior,
    sep = ", "
  )
  df_long <- df_long[order(df_long$tree), ]
  df_long$model_setting <-
    factor(df_long$model_setting, levels = unique(df_long$model_setting))
  df_long$inference_model <-
    factor(df_long$inference_model, levels = unique(df_long$inference_model))
  rownames(df_long) <- mapply(seq_len(nrow(df_long)), FUN = toString)

  message(utils::head(df_long))

  pirouette::pir_plot_from_long(df_long)
}
