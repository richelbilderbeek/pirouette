#' Meaure the error BEAST2 makes from a known phylogeny
#' @inheritParams default_params_doc
#' @return a data frame with errors
#' @export
#' @author Richel J.C. Bilderbeek
pir_run <- function(
  phylogeny,
  alignment_params,
  inference_params,
  model_selections = "generative"
) {
  if (!all(model_selections %in% get_model_selections())) {
    stop("All values of 'model_selections' must be in 'get_model_selections()'")
  }
  df <- data.frame()
  for (model_selection in model_selections) {
    df <- rbind(
      df,
      pir_run_one(
        phylogeny = phylogeny,
        alignment_params = alignment_params,
        inference_params = inference_params,
        model_selection = model_selection
      )
    )
  }
  df
}

#' Meaure the error BEAST2 makes from a known phylogeny
#' using one inference model selection strategy
#' @inheritParams default_params_doc
#' @return a data frame with errors
#' @author Richel J.C. Bilderbeek
#' @noRd
pir_run_one <- function(
  phylogeny,
  alignment_params,
  inference_params,
  model_selection
) {
  testit::assert(length(model_selection) == 1)
  if (!model_selection %in% get_model_selections()) {
    stop("'model_selection' must be in 'get_model_selections()'")
  }

  posterior <- phylo_to_posterior(
    phylogeny = phylogeny,
    alignment_params = alignment_params,
    inference_params = inference_params
  )
  nltts <- nLTT::nltts_diff(tree = phylogeny, trees = posterior$trees)

  df <- data.frame(
    tree = "true",
    inference_model = model_selection,
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
