#' Create a PFF twinning parameter set
#' @inheritParams pirouette::create_twinning_params
#' @export
create_pff_twinning_params <- function(
  rng_seed_twin_tree = pirouette::create_twinning_params()$rng_seed_twin_tree,
  sim_twin_tree_fun =
    pirouette::create_twinning_params()$sim_twin_tree_fun,
  rng_seed_twin_alignment
    = pirouette::create_twinning_params()$rng_seed_twin_alignment,
  sim_twal_fun =
    pirouette::create_twinning_params()$sim_twal_fun,
  twin_tree_filename = peregrine::get_pff_tempfile(
    pattern = "tree_twin_", fileext = ".newick"
  ),
  twin_alignment_filename = peregrine::get_pff_tempfile(
    pattern = "alignment_twin_", fileext = ".fasta"
  ),
  twin_evidence_filename = NA
) {
  twinning_params <- pirouette::create_twinning_params(
    rng_seed_twin_tree = rng_seed_twin_tree,
    sim_twin_tree_fun = sim_twin_tree_fun,
    rng_seed_twin_alignment = rng_seed_twin_alignment,
    sim_twal_fun = sim_twal_fun,
    twin_tree_filename = twin_tree_filename,
    twin_alignment_filename = twin_alignment_filename,
    twin_evidence_filename = twin_evidence_filename
  )
  peregrine::check_pff_twinning_params(twinning_params)
  twinning_params
}
