#' Count mutations
#' @inheritParams default_params_doc
#' @return the number of mutations
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' # Create an AAAA alignent
#' alignment <- ape::as.DNAbin(
#'   x = list(species_1 = strsplit("aaaa", split = "")[[1]])
#' )
#' # Count the number of mutations from AAAA
#' expect_equal(count_n_mutations(alignment, "aaaa"), 0)
#' expect_equal(count_n_mutations(alignment, "acaa"), 1)
#' expect_equal(count_n_mutations(alignment, "acga"), 2)
#' expect_equal(count_n_mutations(alignment, "acgt"), 3)
#' expect_equal(count_n_mutations(alignment, "ccgt"), 4)
#'
#' @export
count_n_mutations <- function(
  alignment,
  root_sequence
) {

  if (class(alignment) != "DNAbin") {
    stop("'alignment' must be of class 'ape::DNAbin'")
  }
  testit::assert(
    get_alignment_sequence_length(alignment) # nolint pirouette function
    == nchar(root_sequence)
  )

  if (is.matrix(alignment)) {
    # We know from
    # https://github.com/richelbilderbeek/pirouette/commit/a96ec3fef34c79e38bed292092c0370e5312c3f6 # nolint indeed long
    # that byrow must be FALSE
    alignment_sequences <- matrix(
      unname(unlist(as.character(alignment))),
      nrow = length(labels(alignment)),
      ncol = ncol(alignment),
      byrow = FALSE
    )
  }
  if (is.list(alignment)) {
    # I assume from
    # https://github.com/richelbilderbeek/pirouette/commit/a96ec3fef34c79e38bed292092c0370e5312c3f6 # nolint indeed long
    # that byrow must be FALSE here as well, but I cannot
    # write a test to verify this
    alignment_sequences <- matrix(
      unname(unlist(as.character(alignment))),
      nrow = length(labels(alignment)),
      ncol = length(as.character(alignment)[[1]]),
      byrow = FALSE
    )
  }

  root_vector <- unlist(strsplit(root_sequence, split = ""))
  if (!all(root_vector %in% c("a", "c", "g", "t"))) {
    stop("'root_sequence' must be one character vector of lowercase nucleotides") # nolint long string
  }

  n_nucleotides <- ncol(alignment_sequences)
  n_nucleotides_as_well <- get_alignment_sequence_length(alignment) # nolint pirouette function

  if (n_nucleotides != n_nucleotides_as_well) {
    stop(
      "Number of nucleotides disagree. \n",
      "n_nucleotides: ", n_nucleotides, " \n",
      "n_nucleotides_as_well: ", n_nucleotides_as_well
    )
  }
  testit::assert(n_nucleotides == get_alignment_sequence_length(alignment)) # nolint pirouette function

  if (n_nucleotides != length(root_vector)) {
    stop(
      "'root_sequence' must have the same length ",
      "as each taxon's sequence length. \n",
      "Number of nucleotides in alignment: ", ncol(alignment_sequences), ". \n",
      "Number of nucleotides in root_vector: ", length(root_vector), ". \n",
      "'root_sequence': ", root_sequence
    )
  }

  n_mutations <- 0
  for (i in 1:nrow(alignment_sequences)) {
    sequence_vector <- alignment_sequences[i, ]
    n_mutations <- n_mutations + sum(root_vector != sequence_vector)
  }
  n_mutations
}
