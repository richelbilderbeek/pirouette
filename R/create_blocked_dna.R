#' Create a 'blocked' DNA sequence, which is a sequence
#' with four equal-sized nucleotide sections
#' @param length number of nucleotides. Must be a multitude of four.
#' @examples
#'   library(testthat)
#'
#'   expect_equal(create_blocked_dna(length = 4), "acgt")
#'   expect_equal(create_blocked_dna(length = 8), "aaccggtt")
#' @seealso Use \link{create_mono_nuc_dna} to create
#'   a mono-nucleotide DNA sequence.
#' @author Richèl J.C. Bilderbeek
#' @export
create_blocked_dna <- function(
  length
) {
  if (!is.numeric(length)) {
    stop("'length' must be numerical")
  }
  if (length < 0) {
    stop("'length' must be a positive non-zero number")
  }
  if (length %% 4 != 0) {
    stop("'length' must be a multitude of four")
  }
  # Nucleotides per base pair
  n_bp <- length / 4
  paste(rep(c("a", "c", "g", "t"), each = n_bp), collapse = "")
}
