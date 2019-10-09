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
  root_sequence,
  verbose = FALSE
) {

  if (class(alignment) != "DNAbin") {
    stop("'alignment' must be of class 'ape::DNAbin'")
  }
  if (pirouette::get_alignment_sequence_length(alignment)
    != nchar(root_sequence)
  ) {
    stop(
      "'root_sequence' must have the same length ",
      "as each taxon's sequence length. \n",
      "'root_sequence' length: ", nchar(root_sequence), " \n",
      "alignment sequence length: ",
        get_alignment_sequence_length(alignment)
    )
  }
  if (!pirouette::is_dna_seq(root_sequence)) {
    stop(
      "'root_sequence' must be one character vector ",
      "of lowercase nucleotides. \n",
      "Actual value: ", root_sequence
    )
  }


  sequences <- pirouette::get_alignment_sequences(
    alignment = alignment, verbose = verbose
  )

  n_mutations <- 0
  for (i in seq_along(sequences)) {
    sequence <- sequences[i]
    n_mutations_here <- sum(
      strsplit(root_sequence, "")[[1]] != strsplit(sequence, "")[[1]]
    )
    if (verbose) {
      print(
        paste0(
          "Sequence ", i, "/", length(sequences),
          " has ", n_mutations_here, " mutations when comparing ",
          "root sequence '", root_sequence,
          "' with taxon sequence '",
          sequence, "'"
        )
      )
    }
    n_mutations <- n_mutations + n_mutations_here
  }
  n_mutations
}
