#' Creates a posterior from the phylogeny
#' @inheritParams default_params_doc
#' @return a posterior of phylogenies
#' @author Richel J.C. Bilderbeek
phylo_to_posterior <- function(
  phylogeny,
  alignment_params,
  inference_params
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
  tryCatch(
    check_inference_params(inference_params),
    error = function(msg) {
      msg <- paste0(
        "'inference_params' must be a set of inference parameters. ",
        msg
      )
      stop(msg)
    }
  )

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

  babette_out <- babette::bbt_run(
    fasta_filename = temp_fasta_filename,
    site_model = inference_params$site_model,
    clock_model = inference_params$clock_model,
    tree_prior = inference_params$tree_prior,
    mrca_prior = inference_params$mrca_prior,
    mcmc = inference_params$mcmc,
    rng_seed = inference_params$rng_seed,
    verbose = inference_params$verbose,
    beast2_path = inference_params$beast2_path
  )

  file.remove(temp_fasta_filename)

  list(
    alignment = alignment,
    # Use c() to convert to multiPhylo. This removes the STATE_x names
    trees = c(babette_out[[grep(x = names(babette_out), pattern = "trees")]]),
    estimates = babette_out$estimates
  )
}
