#' Creates a posterior of phylogenies from a known alignment.
#'
#' @inheritParams default_params_doc
#' @return a list of:
#' \itemize{
#'   \item \code{trees}: the phylogenies in the posterior,
#'     as a \link[ape]{multiphylo}
#'   \item \code{estimates}: the BEAST2 estimates, as a \link{data.frame}
#' }
#' @author Richel J.C. Bilderbeek
alignment_params_to_posterior_trees <- function(# nolint indeed a long name
  alignment_params,
  experiment = create_experiment()
) {
  check_alignment_params(alignment_params) # nolint pirouette function
  check_experiment(experiment) # nolint pirouette function
  testit::assert(file.exists(alignment_params$fasta_filename))
  testit::assert(
    !beautier::is_nested_sampling_mcmc(experiment$inference_model$mcmc)
  )

  print(paste("DEBUG: set verbiose to TRUE"))
  experiment$beast2_options$verbose <- TRUE

  bbt_out <- babette::bbt_run_from_model(
    fasta_filename = alignment_params$fasta_filename,
    inference_model = experiment$inference_model,
    beast2_options = experiment$beast2_options
  )


  print(paste("DEBUG:0 experiment$beast2_options$input_filename:", experiment$beast2_options$input_filename)) # nolint temp
  print(paste("DEBUG:1 length(bbt_out):", length(bbt_out)))
  print(paste("DEBUG:2 names(bbt_out):", names(bbt_out)))
  print(paste("DEBUG:7 length(bbt_out[[1]]):", length(bbt_out[[1]])))
  print(paste("DEBUG:8 class(bbt_out[[1]]):", class(bbt_out[[1]])))
  print("tracerer::count_trees_in_file(experiment$beast2_options$output_trees_filenames)")
  print(tracerer::count_trees_in_file(experiment$beast2_options$output_trees_filenames))

  print("object.size(bbt_out)")
  print(pryr::object_size(bbt_out))
  print("pryr::mem_used()")
  print(pryr::mem_used())

  trees <- c(bbt_out[[grep(x = names(bbt_out), pattern = "trees")]])

  print(paste("DEBUG: length(trees):", length(trees)))

  # Check the number of trees
  mcmc <- experiment$inference_model$mcmc
  testit::assert(!is_nested_sampling_mcmc(mcmc))
  if (mcmc$store_every != -1) {
    expected_n_trees <- 1 + (mcmc$chain_length / mcmc$store_every)
    print("length(trees)")
    print(length(trees))
    print("expected_n_trees")
    print(expected_n_trees)
    testit::assert(length(trees) == expected_n_trees)
  }

  trees
}
