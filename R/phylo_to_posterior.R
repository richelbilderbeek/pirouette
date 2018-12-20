#' Creates a posterior from the phylogeny
#' @inheritParams default_params_doc
#' @return a posterior of phylogenies
#' @author Richel J.C. Bilderbeek
phylo_to_posterior <- function(
  phylogeny,
  alignment_params,
  mcmc,
  site_model = beautier::create_jc69_site_model(),
  clock_model = beautier::create_strict_clock_model(),
  tree_prior = beautier::create_bd_tree_prior(),
  crown_age = NA,
  mrca_distr = NA,
  beast2_rng_seed = 1,
  verbose = FALSE,
  beast2_path = beastier::get_default_beast2_path()
) {
  tryCatch(
    check_alignment_params(alignment_params),
    error = function(msg) {
      msg <- paste0(
        "'alignment_params' must be a set of alignment parameters. ",
        msg
      )
      stop(msg)
    }
  )
  if (!is.na(beast2_rng_seed) && !(beast2_rng_seed > 0)) {
    stop("'beast2_rng_seed' should be NA or non-zero positive")
  }

  # Create alignment, sets alignment RNG seed in 'sim_alignment'
  alignment <- sim_alignment(
    phylogeny = phylogeny,
    alignment_params = alignment_params
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
    fasta_filename = temp_fasta_filename,
    site_model = site_model,
    clock_model = clock_model,
    tree_prior = tree_prior,
    mrca_prior = mrca_prior,
    mcmc = mcmc,
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
