#' Get the sequence length of an alignment
#'
#' It appears DNAbin stores its internals differently for
#' alignments of different sizes. Due to that, this
#' function is more complicated as one would expect
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @examples
#' get_alignment_sequence_length(
#'   alignment = ape::as.DNAbin(
#'     x = list(species_1 = strsplit("aaaa", split = "")[[1]])
#'   )
#' )
#' @export
get_alignment_sequence_length <- function(alignment) {
  testthat::expect_true(class(alignment) == "DNAbin")
  n_nucleotides <- length(alignment[[1]])
  if (is.null(n_nucleotides) || n_nucleotides == 1) {
    n_nucleotides <- ncol(alignment)
  }
  n_nucleotides
}
