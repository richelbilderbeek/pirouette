#' Measure the error BEAST2 makes from a true phylogeny
#'
#'
#' @author Richel J.C. Bilderbeek
#' @export
phylo_to_errors <- function(
  phylogeny,
  alignment_params,
  site_model,
  clock_model,
  tree_prior,
  inference_param
) {
  # Run
  trees <- alignment_params_to_posterior_trees(
    alignment_params = alignment_params,
    site_model = site_model,
    clock_model = clock_model,
    tree_prior = tree_prior,
    inference_param = inference_param
  )

  # Measure error by comparing true tree with BEAST2 posterior trees
  nLTT::nltts_diff(tree = phylogeny, trees = trees)
}
