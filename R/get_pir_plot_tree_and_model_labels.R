#' Internal function to obtain the \link{pir_plot} legend labels
#' @param pir_plot the output created by \code{\link{pir_run}} in the long form
#' @return the \link{pir_plot} legend labels
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @export
get_pir_plot_tree_and_model_labels <- function(pir_out) {# nolint long function name is fine for an internal function

  default_descriptions <- pirouette::get_tree_and_model_descriptions()

  # Convert factor values to human-readable strings
  pir_out$site_model <- plyr::revalue(
    pir_out$site_model, c("JC69" = "JC", "TN93" = "TN"), warn_missing = FALSE)
  pir_out$clock_model <- plyr::revalue(
    pir_out$clock_model,
    c("strict" = "Strict", "relaxed_log_normal" = "RLN"), warn_missing = FALSE
  )
  pir_out$tree_prior <- plyr::revalue(
    pir_out$tree_prior,
    c(
      "yule" = "Yule",
      "birth_death" = "BD",
      "coalescent_bayesian_skyline" = "CBS",
      "coalescent_constant_population" = "CCP",
      "coalescent_exp_population" = "CEP"
    ),
    warn_missing = FALSE
  )


  pir_out$model_setting <- paste(
    pir_out$site_model,
    pir_out$clock_model,
    pir_out$tree_prior,
    sep = ", "
  )
  pir_out$tree_and_model <- paste(
    pir_out$tree,
    pir_out$inference_model,
    sep = "_"
  )
  pir_out$tree_and_model <- as.factor(pir_out$tree_and_model)


  t <- plyr::join(
    x = default_descriptions, y = pir_out, by = "tree_and_model", type = "inner") %>%
    dplyr::select(tree_and_model, description, model_setting)

  t$description <- paste0(t$description, ": ", t$model_setting)
  t$model_setting <- NULL
  t
}
