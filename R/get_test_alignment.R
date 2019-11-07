#' Get an alignment for testing.
#' @inheritParams default_params_doc
#' @examples
#' library(testthat)
#'
#' n_taxa <- 7
#' sequence_length <- 11
#' alignment <- get_test_alignment(
#'   n_taxa = n_taxa,
#'   sequence_length = sequence_length
#' )
#' expect_silent(check_alignment(alignment))
#' expect_equal(n_taxa, get_alignment_n_taxa(alignment))
#' expect_equal(
#'   sequence_length,
#'   get_alignment_sequence_length(alignment)
#' )
#' @export
get_test_alignment <- function(
  n_taxa = 3,
  sequence_length = 4
) {
  # Only have 26 letters in the alphabet
  testit::assert(n_taxa <= 26)
  x <- list()
  for (i in seq_len(n_taxa)) {
    x[[i]] <- rep(
      c("a", "c", "g", "t"),
      each = sequence_length / 4,
      length.out = sequence_length
    )
  }
  alignment <- ape::as.DNAbin(x)
  names(alignment) <- LETTERS[1:n_taxa]
  alignment
}
