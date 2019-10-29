#' Create the parameters for the twinning simulation
#'
#' The site model and clock models will be used and
#' their combination will be called the generative model
#' of the twinning.
#' @param sim_twin_tree_function function to simulate a twin tree with
#' @inheritParams default_params_doc
#' @return a twinning parameter set
#' @examples
#' twinning_params <- create_twinning_params()
#'
#' library(testthat)
#' expect_true("rng_seed_twin_tree" %in% names(twinning_params))
#' expect_true("rng_seed_twin_alignment" %in% names(twinning_params))
#' expect_true("twin_tree_filename" %in% names(twinning_params))
#' expect_silent(check_twinning_params(twinning_params))
#'
#' pir_params <- create_test_pir_params(
#'   twinning_params = twinning_params
#' )
#' expect_silent(check_pir_params(pir_params))
#'
#' if (is_on_ci() &&
#'   rappdirs::app_dir()$os == "unix" &&
#'   is_beast2_installed()) {
#'   pir_out <- pir_run(
#'     phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
#'     pir_params = pir_params
#'   )
#' }
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_twinning_params <- function(
  rng_seed_twin_tree = 0,
  rng_seed_twin_alignment = 0,
  twin_model = "birth_death",
  method = "random_tree",
  n_replicates = 1e4,
  sim_twin_tree_function = create_sim_bd_twin_tree_function(),
  twin_tree_filename = tempfile(
    pattern = "tree_twin_", fileext = ".newick"
  ),
  twin_alignment_filename = tempfile(
    pattern = "alignment_twin_", fileext = ".fasta"
  ),
  twin_evidence_filename =  tempfile(
    pattern = "evidence_twin_", fileext = ".csv"
  )
) {
  twinning_params <- list(
    rng_seed_twin_tree = rng_seed_twin_tree,
    rng_seed_twin_alignment = rng_seed_twin_alignment,
    twin_model = twin_model, # to be obsoleted
    method = method, # to be obsoleted
    n_replicates = n_replicates, # to be obsoleted
    sim_twin_tree_function = sim_twin_tree_function,
    twin_tree_filename = twin_tree_filename,
    twin_alignment_filename = twin_alignment_filename,
    twin_evidence_filename = twin_evidence_filename
  )
  pirouette::check_twinning_params(twinning_params)
  twinning_params
}
