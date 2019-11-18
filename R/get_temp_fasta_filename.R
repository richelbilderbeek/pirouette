#' Get the name for a temporary file to store an
#' alignment in the FASTA format
#' @export
get_temp_fasta_filename <- function() {
  tempfile(
    pattern = "alignment_",
    tmpdir = rappdirs::user_cache_dir(),
    fileext = ".fasta"
  )
}
