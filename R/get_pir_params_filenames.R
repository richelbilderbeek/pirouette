#' @export
get_pir_params_filenames <- function(pir_params) {
  check_pir_params(pir_params)

  filenames <- c(
    get_experiments_filenames(pir_params$experiments),
    pir_params$alignment_params$fasta_filename,
    pir_params$evidence_filename
  )

  if (!beautier::is_one_na(pir_params$twinning_params)) {
    filenames <- c(
      to_twin_filenames(get_experiments_filenames(pir_params$experiments)),
      filenames,
      pir_params$twinning_params$twin_tree_filename,
      pir_params$twinning_params$twin_alignment_filename,
      pir_params$twinning_params$twin_evidence_filename
    )
  }
  filenames
}
