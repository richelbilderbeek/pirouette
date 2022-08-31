#' Create alignment parameters with PFF
#' @inheritParams default_params_doc
#' @export
create_pff_alignment_params <- function(
  root_sequence = pirouette::create_alignment_params()$root_sequence,
  sim_tral_fun =
    pirouette::create_alignment_params()$sim_tral_fun,
  rng_seed = pirouette::create_alignment_params()$rng_seed,
  fasta_filename =
    peregrine::get_pff_tempfile(pattern = "alignment_", fileext = ".fasta")
) {
  alignment_params <- pirouette::create_alignment_params(
    root_sequence = root_sequence,
    sim_tral_fun = sim_tral_fun,
    rng_seed = rng_seed,
    fasta_filename = fasta_filename
  )
  peregrine::check_pff_alignment_params(alignment_params)
  alignment_params
}
