#' Show a DNA alignment as text in FASTA format
#' @export
to_fasta_text <- function(
  alignment,
  alignment_file = tempfile(pattern = "alignment_", fileext = ".fasta")
) {
  ape::write.FASTA(x = alignment, file = alignment_file)
  readLines(alignment_file)
}
