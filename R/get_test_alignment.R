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
