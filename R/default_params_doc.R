#' This function does nothing. It is intended to inherit is parameters'
#' documentation.
#' @param alignment a DNA alignment
#' @param bd_mutation_rate the mutation rate when creating an alignment
#'   from a BD tree
#' @param bd_tree a phylogent of class \code{phylo}, created by a Birth Death
#'   process
#' @param bd_tree_filename name of the file that stores a BD twin tree
#' @param brts set of branching times
#' @param chain_length something
#' @param clock_model Name of the clock model that has
#' to be used for the inference. Valid names are 'strict' and 'rln'.
#' @param crown_age The crown age of the tree.
#' @param fasta_filename name of a FASTA file
#' @param filename the file's name, without the path
#' @param folder_name name of the main folder
#' @param init_speciation_rate a speciation rate
#' @param init_extinction_rate an extinction rate
#' @param lambda per-lineage speciation rate
#' @param mbd_l_matrix the L matrix of an MBD tree
#' @param mbd_mutation_rate the mutation rate when creating an alignment
#'   from a MBD tree
#' @param mbd_tree an MBD tree
#' @param mu per-species extinction rate
#' @param mutation_rate something
#' @param nu the rate at which a multiple-birth specation is triggered
#' @param nu_events the number of nu-triggered events that have to be
#'  present in the simulated tree
#' @param parameters the razzo parameters
#' @param parameter_filename full path to a 'parameters.csv' file
#' @param parameters_filename full path to a 'parameters.csv' file
#' @param phylo a phylogeny
#' @param posterior_trees phylogenetic trees in a BEAST2 posterior,
#'   of class \code{multiphylo}
#' @param precision define the precision of the approximation.
#' @param project_folder_name project folder name,
#'   will be the full path to \code{razzo_project}
#' @param q something
#' @param sample_interval the interval at which the MCMC algorithm
#'   makes a measurement
#' @param sequence_length the length of each DNA sequence in an alignment
#' @param seed a random number generator seed
#' @param sim_pars something
#' @param sim_phylo something
#' @param site_model Name of the site model that
#' has to be used for the inference. Valid names are 'jc69' and 'gtr'.
#' @param sub_chain_length length of the sub-chain used by the Nested Sampling
#'   algorithm to estimate the marginal likelihood
#' @param tree an ultrametric phylogenetic tree of class \code{phylo}
#' @param tree_filename name of the phylogeny file
#' @param trees_filename name of the BEAST2 posterior phylogenies file
#' @param verbose give more output
#' @author Documentation by Giovanni Laudanno,
#'   use of this function by Richel J.C. Bilderbeek
#' @note This is an internal function, so it should be marked with
#'   \code{@noRd}. This is not done, as this will disallow all
#'   functions to find the documentation parameters
#'
default_params_doc <- function(
  alignment,
  bd_mutation_rate,
  bd_tree,
  bd_tree_filename,
  brts,
  chain_length,
  clock_model,
  crown_age,
  fasta_filename,
  filename,
  folder_name,
  init_speciation_rate,
  init_extinction_rate,
  lambda,
  mbd_l_matrix,
  mbd_mutation_rate,
  mbd_tree,
  mu,
  mutation_rate,
  nu,
  nu_events,
  parameters,
  parameter_filename,
  parameters_filename,
  phylo,
  posterior_trees,
  precision,
  project_folder_name,
  q,
  sample_interval,
  seed,
  sequence_length,
  sim_pars,
  sim_phylo,
  site_model,
  sub_chain_length,
  tree,
  tree_filename,
  trees_filename,
  verbose
) {
  # Nothing
}
