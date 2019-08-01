#' Create the parameters for the alignment simulation.
#'
#' These parameters are used in the \link{create_pir_params} function
#' @inheritParams default_params_doc
#' @param rng_seed the random number generator seed as used in the
#'   simulation of an alignment
#' @return a list of alignment parameters
#' @examples
#' library(testthat)
#'
#' # DNA sequence at the root
#' n_base_pairs <- 4
#' root_sequence <- create_blocked_dna(length = n_base_pairs)
#' expect_equal("acgt", root_sequence)
#'
#' # Only specify root sequence and mutation rate, use defaults
#' alignment_params <- create_alignment_params(
#'   root_sequence = root_sequence,
#'   mutation_rate = 0.1
#' )
#'
#' expect_true("root_sequence" %in% names(alignment_params))
#' expect_true("mutation_rate" %in% names(alignment_params))
#'
#' # Use defaults explicitly
#' alignment_params <- create_alignment_params(
#'   root_sequence = root_sequence,
#'   mutation_rate = 0.1,
#'   site_model = create_jc69_site_model(),
#'   clock_model = create_strict_clock_model(),
#'   rng_seed = 0
#' )
#'
#' expect_true("site_model" %in% names(alignment_params))
#' expect_true("clock_model" %in% names(alignment_params))
#' expect_true("rng_seed" %in% names(alignment_params))
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
  mutation_rate = pirouette::create_standard_mutation_rate,
  site_model = beautier::create_jc69_site_model(),
  clock_model = beautier::create_strict_clock_model(),
  rng_seed = 0,
  fasta_filename = tempfile(pattern = "alignment_", fileext = ".fasta")
) {
  alignment_params <- list(
    root_sequence = root_sequence,
    mutation_rate = mutation_rate,
    site_model = site_model,
    clock_model = clock_model,
    rng_seed = rng_seed,
    fasta_filename = fasta_filename
  )
  check_alignment_params(alignment_params = alignment_params) # nolint pirouette function
  alignment_params
}

#' Create testing parameters for the alignment simulation.
#' @inheritParams default_params_doc
#' @return a list of alignment parameters
#' library(testthat)
#'
#' alignment_params <- create_test_alignment_params()
#' expect_silent(check_alignment_params(alignment_params))
#' @author Richèl J.C. Bilderbeek
#' @export
create_test_alignment_params <- function(
  root_sequence = "acgt",
  mutation_rate = pirouette::create_standard_mutation_rate,
  site_model = beautier::create_jc69_site_model(),
  clock_model = beautier::create_strict_clock_model(),
  rng_seed = 0,
  fasta_filename = tempfile(pattern = "alignment_", fileext = ".fasta")
) {
  create_alignment_params(
    root_sequence = root_sequence,
    mutation_rate = mutation_rate,
    site_model = site_model,
    clock_model = clock_model,
    rng_seed = rng_seed,
    fasta_filename = fasta_filename
  ) # nolint pirouette function
}
