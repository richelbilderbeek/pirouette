#' Simulate the true alignment from the true phylogeny
#'
#' @inheritParams default_params_doc
#' @return an alignment of type \link[ape]{DNAbin}
#' @seealso Use \link{create_true_alignment_file} to save
#' the simulated alignment directly to a file
#' @examples
#' library(testthat)
#'
#' # Create the phylogeny to simulate the alignment on
#' n_taxa <- 5
#' true_phylogeny <- ape::rcoal(n_taxa)
#'
#' root_sequence <- "aaaacgt"
#'
#' # Use default settings to create the alignment
#' alignment_params <- create_alignment_params(
<<<<<<< HEAD
#'   sim_true_alignment_function =
#'     get_sim_true_alignment_with_standard_site_model_function(
=======
#'   sim_true_alignment_fun =
#'     get_sim_true_alignment_with_std_site_model_fun(
>>>>>>> b31a67ccf7a115ac420237774dfccbe724a0a7fa
#'       mutation_rate = 1.0
#'   ),
#'   root_sequence = root_sequence
#' )
#'
#' # Simulate the alignment
#' alignment <- sim_true_alignment(
#'    true_phylogeny = true_phylogeny,
#'    alignment_params = alignment_params,
#'  )
#'
<<<<<<< HEAD
#' expect_equal(class(alignment), "DNAbin")
=======
#' expect_silent(check_alignment(alignment))
>>>>>>> b31a67ccf7a115ac420237774dfccbe724a0a7fa
#' expect_equal(nrow(alignment), n_taxa)
#' expect_equal(ncol(alignment), nchar(alignment_params$root_sequence))
#' @author Richèl J.C. Bilderbeek
#' @export
sim_true_alignment <- function(
  true_phylogeny,
  alignment_params = create_alignment_params(),
  verbose = FALSE
) {
  beautier::check_phylogeny(true_phylogeny)
  pirouette::check_alignment_params(alignment_params)
  pirouette::check_reconstructed_phylogeny(true_phylogeny)
  testit::assert(beautier::is_one_bool(verbose))

  set.seed(alignment_params$rng_seed)
<<<<<<< HEAD
  alignment <- alignment_params$sim_true_alignment_function(
=======
  alignment <- alignment_params$sim_true_alignment_fun(
>>>>>>> b31a67ccf7a115ac420237774dfccbe724a0a7fa
    true_phylogeny = true_phylogeny,
    root_sequence = alignment_params$root_sequence
  )
  pirouette::check_alignment(alignment)
  alignment
}
