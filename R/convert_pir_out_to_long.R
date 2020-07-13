#' Convert a \code{pir_out} to its long form
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
  pirouette::check_pir_out(pir_out)

  ##### Data wrangling #####
  # Convert to long form
  df <- pir_out
  first_col_index <- which(names(df) == "error_1")
  testthat::expect_equal(1, length(first_col_index))

  df_long <- tidyr::gather(
    df, "error_index", "error_value", first_col_index:ncol(df)
  )
  testthat::expect_true("error_index" %in% names(df_long))
  df_long$error_index <- NULL
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

  # Remove useless columns
  df_long$inference_model <- NULL
  df_long$tree <- NULL
  df_long$inference_model_weight <- NULL
  df_long$site_model <- NULL
  df_long$clock_model <- NULL
  df_long$tree_prior <- NULL
  df_long$model_setting <- NULL


  rownames(df_long) <- mapply(seq_len(nrow(df_long)), FUN = toString)

  if (isTRUE(verbose)) {
    message(utils::head(df_long))
  }
  df_long
}
