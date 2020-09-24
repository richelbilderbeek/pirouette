#' Create a 'blocked' DNA sequence, which is a sequence
#' with four equal-sized nucleotide sections
#' @param length number of nucleotides
#' @param nucleotide number of nucleotides
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Creates 'aaa'
#' create_mono_nuc_dna(length = 3)
#' 
#' # Creates 'ccccc'
#' create_mono_nuc_dna(nucleotide = "c", length = 5)
#' @seealso Use \link{create_blocked_dna} to create
#'   a DNA sequence of four equally-sized mono-noculeotide block
#' @export
create_mono_nuc_dna <- function(
  length,
  nucleotide = "a"
) {
  if (!beautier::is_one_int(length)) {
    stop("'length' must be numerical")
  }
  if (length < 0) {
    stop("'length' must be a positive non-zero number")
  }
  if (!nucleotide %in% c("a", "c", "g", "t")) {
    stop("'nucleotide' must be a lowercase nucleotide character")
  }

  paste(rep(nucleotide, length), collapse = "")
}
