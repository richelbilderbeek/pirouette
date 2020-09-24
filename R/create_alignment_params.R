#' Create the parameters for the alignment simulation.
#'
#' These parameters are used in the \link{create_pir_params} function
#' @inheritParams default_params_doc
#' @param rng_seed the random number generator seed as used in the
#'   simulation of an alignment
#' @return a list of alignment parameters
#' @examples
#'
#' # DNA sequence at the root
#' n_base_pairs <- 4
#' root_sequence <- create_blocked_dna(length = n_base_pairs)
#'
#' # Only specify root sequence and mutation rate, use defaults
#' alignment_params <- create_alignment_params(
#'   root_sequence = root_sequence
#' )
#'
#' # Use defaults explicitly
#' alignment_params <- create_alignment_params(
#'   root_sequence = root_sequence,
#'   rng_seed = 0
#' )
#'
#' # Create a pirouette parameter set
#' pir_params <- create_test_pir_params(alignment_params = alignment_params)
#'
#' # Run pirouette
#' if (is_on_travis() && is_beast2_installed()) {
#'   pir_out <- pir_run(
#'     phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
#'     pir_params = pir_params
#'   )
#'   pir_plot(pir_out)
#' }
#' @export
#' @author Richèl J.C. Bilderbeek
create_alignment_params <- function(
  root_sequence = pirouette::create_blocked_dna(1000),
  sim_tral_fun =
    pirouette::sim_tral_with_std_nsm,
  rng_seed = 0,
  fasta_filename = pirouette::get_temp_fasta_filename()
) {
  alignment_params <- list(
    root_sequence = root_sequence,
    sim_tral_fun = sim_tral_fun,
    rng_seed = rng_seed,
    fasta_filename = fasta_filename
  )
  pirouette::check_alignment_params(alignment_params)
  alignment_params
}

#' Create testing parameters for the alignment simulation.
#' @inheritParams default_params_doc
#' @return a list of alignment parameters
#'
#' alignment_params <- create_test_alignment_params()
#' expect_silent(check_alignment_params(alignment_params))
#' @author Richèl J.C. Bilderbeek
#' @export
create_test_alignment_params <- function(
  sim_tral_fun =
    pirouette::sim_tral_with_std_nsm,
  root_sequence = "acgt",
  rng_seed = 0,
  fasta_filename = pirouette::get_temp_fasta_filename()
) {
  pirouette::create_alignment_params(
    root_sequence = root_sequence,
    rng_seed = rng_seed,
    fasta_filename = fasta_filename
  )
}
