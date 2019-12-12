#' Create the parameters for the twinning simulation
#'
#' The site model and clock models will be used and
#' their combination will be called the generative model
#' of the twinning.
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
  sim_twin_tree_fun = get_sim_bd_twin_tree_fun(),
  rng_seed_twin_alignment = 0,
  sim_twin_alignment_fun =
    get_sim_twin_alignment_with_std_nsm_fun(),
  twin_tree_filename = to_twin_filename(get_temp_tree_filename()),
  twin_alignment_filename = to_twin_filename(get_temp_fasta_filename()),
  twin_evidence_filename = to_twin_filename(get_temp_evidence_filename())
) {
  twinning_params <- list(
    rng_seed_twin_tree = rng_seed_twin_tree,
    sim_twin_tree_fun = sim_twin_tree_fun,
    rng_seed_twin_alignment = rng_seed_twin_alignment,
    sim_twin_alignment_fun = sim_twin_alignment_fun,
    twin_tree_filename = twin_tree_filename,
    twin_alignment_filename = twin_alignment_filename,
    twin_evidence_filename = twin_evidence_filename
  )
  pirouette::check_twinning_params(twinning_params)
  twinning_params
}
