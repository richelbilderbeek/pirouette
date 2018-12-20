#' Meaure the error BEAST2 makes from a known phylogeny
#' @inheritParams default_params_doc
#' @return a data frame with errors
#' @export
#' @author Richel J.C. Bilderbeek
pir_run <- function(
  phylogeny,
  alignment_params,
  inference_params
) {
  posterior <- phylo_to_posterior(
    phylogeny = phylogeny,
    alignment_params = alignment_params,
    inference_params = inference_params
  )
  nltts <- nLTT::nltts_diff(tree = phylogeny, trees = posterior$trees)

  df <- data.frame(
    tree = "true",
    inference_model = "generative",
    inference_model_weight = NA,
    site_model = "JC69",
    clock_model = "strict",
    tree_prior = "BD"
  )
  error_col_names <- paste0("error_", seq(1, length(nltts)))
  df[, error_col_names] <- 0.0

  from_col_idx <- which(colnames(df) == "error_1")
  df[1, from_col_idx:ncol(df)] <- nltts
  df
}
