#' Create a 'blocked' DNA sequence, which is a sequence
#' with four equal-sized nucleotide sections
#' @param n number of nucleotides. Must be a multitude of four.
#' @author Richel J.C. Bilderbeek
#' @examples
#'   testthat::expect_equal(create_blocked_dna(n = 4), "acgt")
#'   testthat::expect_equal(create_blocked_dna(n = 8), "aaccggtt")
#' @export
create_blocked_dna <- function(n) {

  if (!is.numeric(n)) {
    stop("'n' must be numerical")
  }
  if (n < 0) {
    stop("'n' must be a positive non-zero number")
  }
  if (n %% 4 != 0) {
    stop("'n' must be a multitude of four")
  }
  # Nucleotides per base pair
  n_bp <- n / 4
  paste(rep(c("a", "c", "g", "t"), each = n_bp), collapse = "")
}
