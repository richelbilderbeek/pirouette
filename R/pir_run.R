#' Measure the error BEAST2 makes from a known phylogeny.
#'
#' From a phylogeny of (un)known speciation model,
#' an alignment is created using a known site model and clock model,
#' as given by \code{alignment_params}.
#'
#' @inheritParams default_params_doc
#' @return a data frame with errors, with as many rows as model selection
#'   parameter sets.
#' @seealso
#'   Use \link{pir_plot} to display the output of \link{pir_run} as a
#'   figure.
#'   Use \link{create_test_pir_run_output} to create a test output
#'   of \link{pir_run}
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @examples
#'   library(testthat)
#'
#'   phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
#'
#'   # Select all experiments with 'run_if' is 'always'
#'   experiment <- create_test_gen_experiment()
#'   experiments <- list(experiment)
#'
#'   pir_params <- create_pir_params(
#'     alignment_params = create_test_alignment_params(),
#'     experiments = experiments
#'   )
#'
#'   errors <- NA
#'   if (is_on_ci() && is_beast2_installed()) {
#'     errors <- pir_run(
#'       phylogeny = phylogeny,
#'       pir_params = pir_params
#'     )
#'   } else {
#'     errors <- create_test_pir_run_output()
#'   }
#'
#'   # Return value
#'   expect_true("tree" %in% names(errors))
#'   expect_true(is.factor(errors$tree))
#'   expect_true("true" %in% errors$tree)
#'
#'   expect_true("inference_model" %in% names(errors))
#'   expect_true(is.factor(errors$inference_model))
#'   expect_true("generative" %in% errors$inference_model)
#'
#'   expect_true("inference_model_weight" %in% names(errors))
#'   expect_true(!is.factor(errors$inference_model_weight))
#'
#'   expect_true("site_model" %in% names(errors))
#'   expect_true(is.factor(errors$site_model))
#'   expect_true("JC69" %in% errors$site_model)
#'
#'   expect_true("clock_model" %in% names(errors))
#'   expect_true(is.factor(errors$clock_model))
#'
#'   expect_true("tree_prior" %in% names(errors))
#'   expect_true(is.factor(errors$tree_prior))
#'   expect_true("birth_death" %in% errors$tree_prior ||
#'     "yule" %in% errors$tree_prior
#'   )
#'
#'   expect_true("error_1" %in% names(errors))
#'   expect_true(!is.factor(errors$error_1))
#'
#'   # Errors more than zero
#'   col_first_error <- which(colnames(errors) == "error_1")
#'   col_last_error <- ncol(errors)
#'   expect_true(all(errors[, col_first_error:col_last_error] > 0.0))
#'   n_errors <- col_last_error - col_first_error + 1
#'   expect_true(n_errors < 11) # due to burn-in
#' @export
pir_run <- function(
  phylogeny,
  pir_params = create_pir_params(
    alignment_params = create_alignment_params(
      mutation_rate = create_standard_mutation_rate
    ),
    twinning_params = create_twinning_params()
  )
) {
  # Check the inputs
  beautier::check_phylogeny(phylogeny)
  check_pir_params(pir_params) # nolint pirouette function

  # Higher-level checks
  for (experiment in pir_params$experiments) {
    if (beautier::is_cbs_tree_prior(experiment$inference_model$tree_prior) &&
        ape::Ntip(phylogeny) < 6) {
      stop("Too few taxa to use a Coalescent Bayesian Skyline tree prior")
    }
  }

  # Run for the true tree
  pir_out <- pir_run_true_tree(
    true_phylogeny = phylogeny,
    pir_params = pir_params
  )

  # Run for the twin tree
  if (!beautier::is_one_na(pir_params$twinning_params)) {

    # Create and save twin tree
    twin_tree <- create_twin_tree( # nolint pirouette function
      phylogeny,
      twinning_params = pir_params$twinning_params
    )
    ape::write.tree(
      phy = twin_tree,
      file = pir_params$twinning_params$twin_tree_filename
    )

    # pir_run for the twin
    pir_out_twin <- pir_run_twin_tree( # nolint pirouette function
      twin_phylogeny = twin_tree,
      pir_params = pir_params
    )

    # Twin alignment must have as much mutations as the true alignment
    n_mutations_true <- count_n_mutations(
      alignment = ape::read.FASTA(pir_params$alignment_params$fasta_filename),
      root_sequence = pir_params$alignment_params$root_sequence
    )
    n_mutations_twin <- count_n_mutations(
      alignment = ape::read.FASTA(
        pir_params$twinning_params$twin_alignment_filename
      ),
      root_sequence = pir_params$alignment_params$root_sequence
    )
    testit::assert(n_mutations_true == n_mutations_twin)

    pir_out <- rbind(pir_out, pir_out_twin)
  }
  pir_out
}
