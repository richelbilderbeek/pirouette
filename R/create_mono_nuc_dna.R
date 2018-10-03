#' Create a 'blocked' DNA sequence, which is a sequence
#' with four equal-sized nucleotide sections
#' @param length number of nucleotides
#' @param nucleotide number of nucleotides
#' @author Richel J.C. Bilderbeek
#' @examples
#'   testthat::expect_equal(create_mono_nuc_dna(length = 3), "aaa")
#'   testthat::expect_equal(
#'     create_mono_nuc_dna(nucleotide = "c", length = 5),
#'     "ccccc"
#'   )
#' @export
create_mono_nuc_dna <- function(
  length,
  nucleotide = "a"
) {
  if (!is.numeric(length)) {
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
