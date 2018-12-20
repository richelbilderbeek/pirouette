#' This function does nothing. It is intended to inherit is parameters'
#' documentation.
#' @param alignment a DNA alignment
#' @param alignment_rng_seed The random number generator seed used
#'   to generate an alignment
#' @param bd_mutation_rate the mutation rate when creating an alignment
#'   from a BD tree
#' @param bd_tree a phylogent of class \code{phylo}, created by a Birth Death
#'   process
#' @param bd_tree_filename name of the file that stores a BD twin tree
#' @param beast2_path Path to the
#'   BEAST2 jar file (\code{beast.jar})
#'   or BEAST2 binary file '(\code{beast})'.
#'   Use \link[beastier]{get_default_beast2_jar_path} for the default
#'   BEAST2 jar file path.
#'   Use \link[beastier]{get_default_beast2_bin_path} for the default
#'   BEAST2 binary file path.
#' @param beast2_rng_seed The random number generator seed used by BEAST2
#' @param brts set of branching times
#' @param chain_length something
#' @param clock_model a clock model,
#'   as created by \link[beautier]{create_clock_model}
#' @param crown_age the fixed crown age of the posterior. Set to NA
#'   to let it be estimated
#' @param fasta_filename name of a FASTA file
#' @param filename the file's name, without the path
#' @param folder_name name of the main folder
#' @param init_speciation_rate a speciation rate
#' @param init_extinction_rate an extinction rate
#' @param lambda per-lineage speciation rate
#' @param mcmc MCMC options, as created by \link[beautier]{create_mcmc}
#' @param mbd_l_matrix the L matrix of an MBD tree
#' @param mbd_mutation_rate the mutation rate when creating an alignment
#'   from a MBD tree
#' @param mbd_tree an MBD tree
#' @param mrca_distr if MRCA prior used on all taxa.
#'   Set to NA to not use an MRCA prior
#' @param mu per-species extinction rate
#' @param mutation_rate the mutation rate per base pair per time unit
#' @param nu the rate at which a multiple-birth specation is triggered
#' @param nu_events the number of nu-triggered events that have to be
#'  present in the simulated tree
#' @param parameters the razzo parameters
#' @param parameter_filename full path to a 'parameters.csv' file
#' @param parameters_filename full path to a 'parameters.csv' file
#' @param phylo a phylogeny of class \link[ape]{phylo}
#' @param phylogeny a phylogeny of class \link[ape]{phylo}
#' @param posterior_trees phylogenetic trees in a BEAST2 posterior,
#'   of class \code{multiphylo}
#' @param precision define the precision of the approximation.
#' @param project_folder_name project folder name,
#'   will be the full path to \code{razzo_project}
#' @param root_sequence the DNA sequence at the root of the phylogeny.
#'   By default, this will consist out of only adenine
#' @param sample_interval the interval at which the MCMC algorithm
#'   makes a measurement
#' @param sequence_length the length of each DNA sequence in an alignment
#' @param seed a random number generator seed
#' @param sim_pars something
#' @param sim_phylo something
#' @param site_model a nucleotide substitution model,
#'   as created by \link[beautier]{create_site_model}
#' @param sub_chain_length length of the sub-chain used by the Nested Sampling
#'   algorithm to estimate the marginal likelihood
#' @param tree an ultrametric phylogenetic tree of class \link[ape]{phylo}
#' @param tree_prior a tree prior,
#'   as created by \link[beautier]{create_tree_prior}
#' @param tree_filename name of the phylogeny file
#' @param trees_filename name of the BEAST2 posterior phylogenies file
#' @param verbose if TRUE, show more output
#' @author Documentation by Giovanni Laudanno,
#'   use of this function by Richel J.C. Bilderbeek
#' @note This is an internal function, so it should be marked with
#'   \code{@noRd}. This is not done, as this will disallow all
#'   functions to find the documentation parameters
#'
default_params_doc <- function(
  alignment,
  alignment_rng_seed,
  bd_mutation_rate,
  bd_tree,
  bd_tree_filename,
  beast2_path,
  beast2_rng_seed,
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
  mcmc,
  mrca_distr,
  mu,
  mutation_rate,
  nu,
  nu_events,
  parameters,
  parameter_filename,
  parameters_filename,
  phylo,
  phylogeny,
  posterior_trees,
  precision,
  project_folder_name,
  root_sequence,
  sample_interval,
  seed,
  sequence_length,
  sim_pars,
  sim_phylo,
  site_model,
  sub_chain_length,
  tree,
  tree_filename,
  tree_prior,
  trees_filename,
  verbose
) {
  # Nothing
}
