#' Creates a posterior from the phylogeny
#' @param phylogeny a phylogeny
#' @param sequence_length the number of basepair the simulated DNA
#'   alignment consists of
#' @param mutation_rate the mutation rate per base pair per time unit
#' @param chain_length MCMC chain length
#' @param crown_age the crown age
#' @param rng_seed The random number generator seed used by BEAST2
#' @param verbose if TRUE, show more output
#' @param beast_jar_path Where the jar 'beast.jar' can be found
#' @return a posterior of phylogenies
#' @export
#' @author Richel J.C. Bilderbeek
run <- function(
  phylogeny,
  sequence_length,
  mutation_rate,
  chain_length,
  crown_age,
  rng_seed = 0,
  verbose = FALSE,
  beast_jar_path = beastier::get_default_beast2_jar_path()
) {
  # Create alignment
  alignment <- sim_alignment(
    phylogeny = phylogeny,
    sequence_length = sequence_length,
    mutation_rate = mutation_rate
  )
  # Save alignment to file
  temp_fasta_filename <- paste0("tmp_pirouette_run.fasta")
  phangorn::write.phyDat(
    alignment,
    file = temp_fasta_filename,
    format = "fasta"
  )

  out <- babette::run(
    fasta_filenames = temp_fasta_filename,
    site_models = beautier::create_jc69_site_model(),
    clock_models = beautier::create_strict_clock_model(),
    mrca_priors = NA,
    mcmc = beautier::create_mcmc(chain_length = chain_length),
    tree_priors = beautier::create_bd_tree_prior(),
    posterior_crown_age = crown_age,
    rng_seed = rng_seed,
    cleanup = TRUE,
    verbose = verbose
  )

  file.remove(temp_fasta_filename)

  posterior <- out[[grep(x = names(out), pattern = "trees")]]
  posterior
}
