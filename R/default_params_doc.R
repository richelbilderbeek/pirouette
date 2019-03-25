#' This function does nothing. It is intended to inherit is parameters'
#' documentation.
#' @param alignment a DNA alignment
#' @param alignment_params parameters to simulate an alignment,
#'   as can be created by \link{create_alignment_params}
#' @param alignment_rng_seed The random number generator seed used
#'   to generate an alignment
#' @param base_frequencies the four base frequencies (a, c, g, t) to be
#'   specified to create the rate matrix (i.e. Q matrix)
#'   used to simulate alignments
#' @param bd_mutation_rate the mutation rate when creating an alignment
#'   from a BD tree
#' @param bd_tree a phylogent of class \link[ape]{phylo},
#'   created by a Birth Death process
#' @param bd_tree_filename name of the file that stores a BD twin tree
#' @param beast2_bin_path path to BEAST2 binary file. The use of the
#'   binary BEAST2 file is required for estimation of the evidence (aka
#'   marginal likelihood). The default BEAST2 binary path can be
#'   obtained using \link[beastier]{get_default_beast2_bin_path}
#' @param beast2_input_filename path of the BEAST2 configuration file.
#'   By default, this file is put in a temporary folder with a random filename,
#'   as the user needs not read it: it is used as input of BEAST2.
#'   Specifying a \code{beast2_input_filename} allows
#'   to store that file in a more permanently stored location.
#' @param beast2_options BEAST2 options,
#'   as can be created by \link[beastier]{create_beast2_options}
#' @param beast2_options_inference BEAST2 options,
#'   as can be created by \link[beastier]{create_beast2_options}.
#'   The MCMC must be a normal MCMC,
#'   as can be created by \link[beautier]{create_mcmc}.
#' @param beast2_options_est_evidence BEAST2 options to estimate
#'   the evidence (aka marginal likelihood),
#'   as can be created by \link[beastier]{create_beast2_options}.
#'   The MCMC must be a Nested Sampling MCMC,
#'   as can be created by \link[beautier]{create_nested_sampling_mcmc}.
#' @param beast2_output_log_filename name of the log file created by BEAST2,
#'   containing the parameter estimates in time.
#'   By default, this file is put a temporary folder with a random filename,
#'   as the user needs not read it.
#'   Specifying a beast2_output_log_filename allows to store that file
#'   in a more permanently stored location.
#' @param beast2_output_state_filename name of the final state file
#'   created by BEAST2, containing the operator acceptances.
#'   By default, this file is put a temporary folder with a random filename,
#'   as the user needs not read it.
#'   Specifying a beast2_output_state_filename allows to store
#'   that file in a more permanently stored location.
#' @param beast2_output_trees_filename name of a trees files
#'   created by BEAST2.
#'   By default, this file is put a temporary folder with a random filename,
#'   as the user needs not read it: its content is parsed and
#'   compared to a true phylogeny to obtain the inference errors.
#'   Specifying \code{beast2_output_trees_filename} allows to store
#'   this file in a more permanently stored location.
#' @param beast2_output_trees_filenames	name of the one or more trees files
#'   created by BEAST2, one per alignment.
#'   By default, these files are put a temporary folder with a random filename,
#'   as the user needs not read it: its content is parsed and
#'   compared to a true phylogeny to obtain the inference errors.
#'   Specifying \code{beast2_output_trees_filenames} allows to store
#'   these one or more files in a more permanently stored location.
#' @param beast2_path Path to the
#'   BEAST2 jar file (\code{beast.jar})
#'   or BEAST2 binary file '(\code{beast})'.
#'   Use \link[beastier]{get_default_beast2_jar_path} for the default
#'   BEAST2 jar file path.
#'   Use \link[beastier]{get_default_beast2_bin_path} for the default
#'   BEAST2 binary file path.
#' @param beast2_rng_seed The random number generator seed used by BEAST2
#' @param brts numeric vector of (all postive) branching times,
#'   in time units before the present. Assuming no stem, the heighest
#'   value equals the crown age.
#' @param burn_in_fraction the fraction of the posterior trees (starting
#'   from the ones generated first)
#'   that will be discarded,
#'   must be a value from 0.0 (keep all), to 1.0 (discard all).
#' @param chain_length something
#' @param clock_model a clock model,
#'   as created by \link[beautier]{create_clock_model}
#' @param clock_models a list of one or more clock models,
#'   as created by \link[beautier]{create_clock_model}
#' @param clock_model_name name of a clock model
#' @param crown_age the fixed crown age of the posterior. Set to NA
#'   to let it be estimated
#' @param do_measure_evidence boolean to indicate if the
#'   evidence (aka marginal likelihood) of an experiment must be
#'   measured
#' @param epsilon	measure of relative accuracy when estimating a model's
#'   evidence (also known as marginal likelihood).
#'   Smaller values result in more precise estimations, that take
#'   longer to compute
#' @param error_function function that determines the error between
#'   a given phylogeny and a the trees in a Bayesian posterior.
#'   The function must have two arguments:
#'   \itemize{
#'     \item the one given phylogeny, of class \link[ape]{phylo}
#'     \item one or more posterior trees, of class \link[ape]{multiphylo}
#'   }
#'   The function must return as many errors as there are posterior
#'   trees given. The error must be lowest between identical trees.
#'   Example functions are:
#'   \itemize{
#'     \item \link{get_gamma_error_function}: use the absolute difference
#'       in gamma statistic
#'     \item \link{get_nltt_error_function}: use the nLTT statistic
#'   }
#' @param error_measure_params parameter set to specify how the
#'   error between the given phylogeny and the Bayesian
#'   posterior is determined.
#'   Use \link{create_error_measure_params} to create such
#'   a parameter set
#' @param errors a numeric vector of (positive) Bayesian inference errors.
#'   Use NA if these are not measured (yet)
#' @param errors_filename baseline name for errors filenames
#' @param est_evidence_mcmc MCMC used in the estimation of
#'   the evidence (aka marginal likelihood).
#'   The MCMC must be a Nested Sampling MCMC,
#'   as can be created by \link[beautier]{create_nested_sampling_mcmc}.
#' @param evidence_epsilon relative error in estimating the
#'   evidence (aka marginal likelihood).
#' @param evidence_filename filename to store the estimated
#'   evidences (aka marginal likelihoods)
#' @param exclude_model an inference model that has to be excluded, as can be
#'   created by \link[beautier]{create_inference_model}
#' @param experiment a \link{pirouette} experiment,
#'   as can be created by \link{create_experiment}
#' @param experiments a list of one or more \link{pirouette} experiments,
#'   as can be created by \link{create_experiment}. If more than one experiment
#'   is provided and a "generative" experiment is part of them, the "generative"
#'   one has to be the first in the list.
#' @param fasta_filename name of a FASTA file
#' @param filename the file's name, without the path
#' @param folder_name name of the main folder
#' @param ideal_method method to generate the "ideal" tree
#' @param inference_conditions conditions under which the inference model
#'   is used in the inference
#' @param inference_model an inference model, which is a combination
#'   of site model, clock model, tree prior and BEAST2 input and
#'   input filenames.
#' @param init_speciation_rate a speciation rate
#' @param init_extinction_rate an extinction rate
#' @param lambda per-lineage speciation rate
#' @param log_evidence the natural logarithm of the evidence (aka marginal
#'   likelihood). Can be NA if this is not measured
#' @param marg_lik_filename name of the file the marginal
#'   likelihoods (also known as 'evidences') are saved to
#' @param marg_liks a data frame with marginal likelihoods/evidences.
#'   A test data frame can be created by \link{create_test_marg_liks}
#' @param max_evidence_epsilon set the maximum acceptable threshold for the
#'   parameter \code{evidence_epsilon}
#' @param mcmc MCMC options, as created by \link[beautier]{create_mcmc}
#' @param mbd_l_matrix the L matrix of an MBD tree
#' @param mbd_mutation_rate the mutation rate when creating an alignment
#'   from a MBD tree
#' @param mbd_tree an MBD tree
#' @param method determines how to create the twin tree
#' \itemize{
#'     \item 'random_tree' just produces a random tree;
#'     \item 'max_clade_cred' simulates \code{n_replicates} trees and
#'       uses \link[phangorn]{maxCladeCred} to create a consensus tree;
#'     \item 'max_likelihood' simulates \code{n_replicates} trees
#'      and selects the most likely;
#'   }
#' @param model_selection one ways to select the models used in
#'   inference, for example, \code{generative} picks the generative
#'   model, where \code{most_evidence} picks the model with most
#'   evidence. See \link{get_model_selections} for a list of
#' @param model_type type of inference model supplied for an experiment.
#'   Possible values:
#'   \itemize{
#'     \item \code{generative}: the inference model is (or is assumed to be)
#'       the inference model underlying the phylogeny
#'     \item \code{candidate}: the inference model is a candidate model,
#'       that competes with other models for having the most
#'       evidence (aka highest marginal likelihood)
#'   }
#' @param mrca_prior an MRCA prior,
#'   as created by \link[beautier]{create_mrca_prior}
#' @param mu per-species extinction rate
#' @param mutation_rate the mutation rate per base pair per time unit
#' @param n_0 number of starting species
#' @param n_taxa number of tree tips
#' @param n_replicates number of replicas to evaluate in order to create the
#'   twin tree
#' @param nu the rate at which a multiple-birth specation is triggered
#' @param nu_events the number of nu-triggered events that have to be
#'  present in the simulated tree
#' @param parameter_filename full path to a 'parameters.csv' file
#' @param parameters_filename full path to a 'parameters.csv' file
#' @param phylo a phylogeny of class \link[ape]{phylo}
#' @param phylogeny a phylogeny of class \link[ape]{phylo}
#' @param pir_params the parameters of \link[pirouette]{pirouette}.
#'   They are created by \link{create_pir_params}.
#' @param pir_out the output of \link{pir_run_tree} applied on the original tree
#' @param posterior_trees phylogenetic trees in a BEAST2 posterior,
#'   of class \code{multiphylo}
#' @param precision define the precision of the approximation.
#' @param project_folder_name project folder name
#' @param result results from measurements. These are:
#'   \itemize{
#'     \item log_evidence the natural logarithm of the evidence (aka marginal
#'       likelihood). Can be NA if this is not measured
#'     \item weight the weight of the model, compared to other (candidate)
#'       models. This weight will be between 0.0 (there is no evidence for
#'       this model) to 1.0 (all evidence indicates this is the best model).
#'       A weight of NA denotes that the weight is not measured
#'     \item errors a numeric vector of (positive) Bayesian inference errors.
#'       Will be NA if these are not measured.
#'   }
#' @param rng_seed a random number generator seed
#' @param root_sequence the DNA sequence at the root of the phylogeny.
#'   By default, this will consist out of an equal amount of each letter
#' @param run_if the condition for an experiment's inference model to be run.
#'   Possible values:
#'   \itemize{
#'     \item \code{always}: always
#'     \item \code{best_candidate}: if the inference model is the
#'       candidate model with the most evidence (aka highest marginal
#'       likelihood)
#'   }
#' @param run_experiment one \link{pirouette} run experiment.
#'   A run experiment has these attributes:
#'   \itemize{
#'     \item experiment the (original) experiment
#'     \item true_result the result of running the original experiment on
#'       the true phylogeny
#'     \item twin_result the result of running the original experiment on
#'       the twin phylogeny
#'   }
#' @param run_experiments a list of one or more \link{pirouette} run experiments
#' @param sample_interval the interval at which the MCMC algorithm
#'   makes a measurement
#' @param sequence_length the length of each DNA sequence in an alignment
#' @param seed a random number generator seed
#' @param sim_pars something
#' @param sim_phylo something
#' @param site_model a nucleotide substitution model,
#'   as created by \link[beautier]{create_site_model}
#' @param site_models a list of one or more site models,
#'   as created by \link[beautier]{create_site_model}
#' @param site_model_name name of a site model
#' @param sub_chain_length length of the sub-chain used by the Nested Sampling
#'   algorithm to estimate the marginal likelihood
#' @param sum_lamu is the sum lambda + mu
#' @param t_0 starting time of a tree
#' @param tree an ultrametric phylogenetic tree of class \link[ape]{phylo}
#' @param tree_model model used to simulate the tree
#' @param tree_prior a tree prior,
#'   as created by \link[beautier]{create_tree_prior}
#' @param tree_priors a list of one or more tree priors,
#'   as created by \link[beautier]{create_tree_prior}
#' @param tree_prior_name name of a tree prior
#' @param tree_type type of tree, can be \code{true} for the true
#'   phylogeny, and \code{twin} for its twin tree
#' @param tree_filename name of the phylogeny file
#' @param true_result result obtained from using the true tree
#' @param twin_alignment_filename name of the FASTA file the twin
#'   alignment will be saved to
#' @param twin_evidence_filename filename to store the estimated
#'   evidences (aka marginal likelihoods) of the twin tree
#' @param twin_model the model you want to use to generate the twin tree:
#'   \itemize{
#'     \item \code{birth_death}: birth death
#'     \item \code{yule}: Yule or pure-birth
#'   }
#'   See \link{get_twin_models} to see all possible
#'   values of \code{twin_model}
#' @param twin_result result obtained from using the twin tree
#' @param twin_tree_filename  name of the (\code{.newick}) file the twin
#'   tree will be saved to
#' @param twinning_params can be \code{NA} if no twinning is desired,
#'   or can be the twinning parameters,
#'   as can be created by \link{create_twinning_params}
#' @param type one or more ways to select the models used in inference:
#'   \itemize{
#'     \item \code{"generative"}: pick the generative model
#'     \item \code{most_evidence} picks the model with most evidence
#'   }
#'   See \link{get_model_selections} for a list.
#' @param use_new_interface set to TRUE to use a new interface
#' @param verbose if TRUE, show more output
#' @param weight the weight of the model, compared to other (candidate)
#'   models. This weight will be between 0.0 (there is no evidence for
#'   this model) to 1.0 (all evidence indicates this is the best model).
#'   A weight of NA denotes that the weight is not measured
#' @author Documentation by Giovanni Laudanno,
#'   use of this function by Richel J.C. Bilderbeek
#' @note This is an internal function, so it should be marked with
#'   \code{@noRd}. This is not done, as this will disallow all
#'   functions to find the documentation parameters
default_params_doc <- function(
  alignment,
  alignment_params,
  alignment_rng_seed,
  base_frequencies,
  bd_mutation_rate,
  bd_tree,
  bd_tree_filename,
  beast2_bin_path,
  beast2_input_filename,
  beast2_options,
  beast2_options_inference,
  beast2_options_est_evidence,
  beast2_output_log_filename,
  beast2_output_state_filename,
  beast2_output_trees_filename,
  beast2_output_trees_filenames,
  beast2_path,
  beast2_rng_seed,
  brts,
  burn_in_fraction,
  chain_length,
  clock_model, clock_models,
  clock_model_name,
  crown_age,
  do_measure_evidence,
  epsilon,
  error_function,
  error_measure_params,
  errors,
  errors_filename,
  est_evidence_mcmc,
  evidence_epsilon,
  evidence_filename,
  exclude_model,
  experiment, experiments,
  fasta_filename,
  filename,
  folder_name,
  ideal_method,
  inference_model,
  inference_conditions,
  init_speciation_rate,
  init_extinction_rate,
  lambda,
  log_evidence,
  marg_lik_filename,
  marg_liks,
  max_evidence_epsilon,
  mbd_l_matrix,
  mbd_mutation_rate,
  mbd_tree,
  mcmc,
  method,
  model_selection,
  model_type,
  mrca_prior,
  mu,
  mutation_rate,
  n_0,
  n_taxa,
  n_replicates,
  nu,
  nu_events,
  parameter_filename,
  parameters_filename,
  phylo,
  phylogeny,
  pir_params,
  pir_out,
  posterior_trees,
  precision,
  project_folder_name,
  result,
  rng_seed,
  root_sequence,
  run_experiment,
  run_experiments,
  run_if,
  sample_interval,
  seed,
  sequence_length,
  sim_pars,
  sim_phylo,
  site_model,
  site_models,
  site_model_name,
  sub_chain_length,
  sum_lamu,
  t_0,
  tree,
  tree_filename,
  tree_model,
  tree_prior, tree_priors,
  tree_prior_name,
  tree_type,
  true_result,
  twin_alignment_filename,
  twin_evidence_filename,
  twin_model,
  twin_result,
  twin_tree_filename,
  twinning_params,
  type,
  use_new_interface,
  verbose,
  weight
) {
  # Nothing
}
