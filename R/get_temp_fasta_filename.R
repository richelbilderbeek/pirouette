#' Get the name for a temporary file to store an
#' alignment in the FASTA format
#' @return one string
#' @export
get_temp_fasta_filename <- function() {
  beautier::get_beautier_tempfilename(
    pattern = "alignment_",
    fileext = ".fasta"
  )
}
