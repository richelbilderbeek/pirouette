#' Creates a posterior from the phylogeny
#' @param phylogeny a phylogeny
#' @param sequence_length the number of basepair the simulated DNA
#'   alignment consists of
#' @param root_sequence the DNA sequence at the root of the phylogeny.
#'   By default, this will consist out of only adenine
#' @param mutation_rate the mutation rate per base pair per time unit
#' @param mcmc MCMC options, as created by \link[beautier]{create_mcmc}
#' @param crown_age the fixed crown age of the posterior. Set to NA
#'   to let it be estimated
#' @param mrca_distr if MRCA prior used on all taxa.
#'   Set to NA to not use an MRCA prior
#' @param site_models one or more nucleotide substitution models,
#'   as created by \link[beautier]{create_site_model}
#' @param clock_models one or more clock models,
#'   as created by \link[beautier]{create_clock_model}
#' @param tree_priors one or more tree prios,
#'   as created by \link[beautier]{create_tree_prior}
#' @param alignment_rng_seed The random number generator seed used
#'   to generate an alignment
#' @param beast2_rng_seed The random number generator seed used by BEAST2
#' @param verbose if TRUE, show more output
#' @param beast2_path Path to the BEAST2 binary (\code{beast})
#'   or jar file (\code{beast.jar})
#' @return a posterior of phylogenies
#' @examples
#'  # Create a phylogeny
#'  phylogeny <- ape::read.tree(text = "(((A:1,B:1):1,C:2):1,D:3);")
#'
#'  # Create a BEAST2 posterior from this phylogeny's simulated alignment.
#'  # Estimate the crown age
#'  out <- pir_run(
#'    phylogeny = phylogeny,
#'    sequence_length = 10,
#'    mutation_rate = 0.1,
#'    mcmc = beautier::create_mcmc(chain_length = 2000)
#'  )
#'  # Trees are estimated after 0, 1000 and 2000 MCMC states
#'  testit::assert(length(out$trees) == 3)
#'  testit::assert(nrow(out$estimates) == 3)
#'
#'  # Create a BEAST2 posterior from this phylogeny's simulated alignment,
#'  # now assume a (close-to) fixed crown age
#'  crown_age <- 15.0
#'  out <- pir_run(
#'    phylogeny = phylogeny,
#'    sequence_length = 10,
#'    mutation_rate = 0.1,
#'    mcmc = beautier::create_mcmc(chain_length = 2000),
#'    mrca_distr = beautier::create_normal_distr(
#'      mean = create_mean_param(crown_age),
#'      sigma = create_sigma_param(0.001)
#'    )
#'  )
#'  testit::assert(length(out$trees) == 3)
#'  testit::assert(nrow(out$estimates) == 3)
#'  # Estimated crown age (called 'TreeHeight' by BEAST2) is not
#'  # yet estimated correctly. Increase the MCMC length to achieve this
#'  testit::assert(!all(abs(crown_age - out$estimates$TreeHeight) == crown_age))
#' @export
#' @author Richel J.C. Bilderbeek
pir_run <- function(
  phylogeny,
  sequence_length = NULL,
  root_sequence,
  mutation_rate,
  mcmc,
  site_models = beautier::create_jc69_site_model(),
  clock_models = beautier::create_strict_clock_model(),
  tree_priors = beautier::create_bd_tree_prior(),
  crown_age = NA,
  mrca_distr = NA,
  alignment_rng_seed = 0,
  beast2_rng_seed = 1,
  verbose = FALSE,
  beast2_path = beastier::get_default_beast2_path()
) {
  if (!pir_is_dna_seq(root_sequence)) {
    stop("'root_sequence' should be a lower-case DNA character string")
  }
  if (is.numeric(sequence_length) && nchar(root_sequence) != sequence_length) {
    stop(
      "'sequence_length' must be NULL ",
      "or equal the number of characters in 'root_sequence'"
    )
  }
  if (is.numeric(sequence_length)) {
    warning(
      "'sequence_length' will be removed from the interface ",
      "in a future version. The number of characters in 'root_sequence' ",
      "will be used instead"
    )
  }

  if (!is.na(beast2_rng_seed) && !(beast2_rng_seed > 0)) {
    stop("'beast2_rng_seed' should be NA or non-zero positive")
  }
  # Create alignment
  set.seed(alignment_rng_seed)
  alignment <- sim_alignment(
    phylogeny = phylogeny,
    sequence_length = sequence_length,
    root_sequence = root_sequence,
    mutation_rate = mutation_rate
  )
  # Save alignment to file
  temp_fasta_filename <- tempfile(pattern = "pirouette_", fileext = ".fasta")
  phangorn::write.phyDat(
    alignment,
    file = temp_fasta_filename,
    format = "fasta"
  )

  mrca_prior <- NA
  if (beautier:::is_distr(mrca_distr)) {
    mrca_prior <- beautier::create_mrca_prior(
      alignment_id = beautier::get_alignment_id(temp_fasta_filename),
      taxa_names = beautier::get_taxa_names(temp_fasta_filename),
      is_monophyletic = TRUE,
      mrca_distr = mrca_distr
    )
  }

  babette_out <- babette::bbt_run(
    fasta_filenames = temp_fasta_filename,
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors,
    mrca_priors = mrca_prior,
    mcmc = mcmc,
    posterior_crown_age = crown_age,
    rng_seed = beast2_rng_seed,
    cleanup = TRUE,
    verbose = verbose,
    beast2_path = beast2_path
  )

  file.remove(temp_fasta_filename)

  list(
    alignment = alignment,
    # Use c() to convert to multiPhylo. This removes the STATE_x names
    trees = c(babette_out[[grep(x = names(babette_out), pattern = "trees")]]),
    estimates = babette_out$estimates
  )
}
