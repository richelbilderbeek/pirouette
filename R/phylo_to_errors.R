#' Measure the error BEAST2 makes from a true phylogeny.
#'
#' The supplied phylogeny (\code{phylogeny}) is the true/known phylogeny.
#' From the phylogeny, already an alignment is simulated and saved
#' as a FASTA file with name \code{alignment_params$fasta_filename}.
#'
#' Using the setup itemized below, the posterior phylogengies are generated:
#' \itemize{
#'    \item a site model (\code{site_model})
#'    \item a clock model (\code{clock_model})
#'    \item a tree prior (\code{tree_prior})
#'    \item an MRCA prior (\code{inference_param$mrca_prior})
#'    \item an MCMC setup (\code{inference_param$mcmc})
#'    \item a BEAST2 RNG seed (\code{inference_param$rng_seed})
#'    \item a BEAST2 path (\code{inference_param$beast2_path})
#' }
#'
#' The posterior phylogenies are compared to the true/known phylogeny
#' using the nLTT statistics. These nLTT statistics, all with values
#' betweem (including) zero and (including) one, are returned.
#' @inheritParams default_params_doc
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