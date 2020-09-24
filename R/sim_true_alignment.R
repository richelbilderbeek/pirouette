#' Simulate the true alignment from the true phylogeny
#'
#' @inheritParams default_params_doc
#' @return an alignment of type \link[ape]{DNAbin}
#' @seealso Use \link{create_tral_file} to save
#' the simulated alignment directly to a file
#' @examples
#'
#' # Create the phylogeny to simulate the alignment on
#' n_taxa <- 5
#' true_phylogeny <- ape::rcoal(n_taxa)
#'
#' root_sequence <- "aaaacgt"
#'
#' # Use default settings to create the alignment
#' alignment_params <- create_alignment_params(
#'   sim_tral_fun =
#'     get_sim_tral_with_std_nsm_fun(
#'       mutation_rate = 1.0
#'   ),
#'   root_sequence = root_sequence
#' )
#'
#' # Simulate the alignment
#' alignment <- sim_true_alignment(
#'   true_phylogeny = true_phylogeny,
#'   alignment_params = alignment_params,
#' )
#' check_alignment(alignment)
#' @author Richèl J.C. Bilderbeek
#' @export
sim_true_alignment <- function(
  true_phylogeny,
  alignment_params = pirouette::create_alignment_params(),
  verbose = FALSE
) {
  beautier::check_phylogeny(true_phylogeny)
  pirouette::check_alignment_params(alignment_params)
  pirouette::check_reconstructed_phylogeny(true_phylogeny)
  testit::assert(beautier::is_one_bool(verbose))

  set.seed(alignment_params$rng_seed)
  alignment <- alignment_params$sim_tral_fun(
    true_phylogeny = true_phylogeny,
    root_sequence = alignment_params$root_sequence
  )
  pirouette::check_alignment(alignment)
  alignment
}
