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
#'
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
  alignment_sequences <- strsplit(
    gsub(
      unname(unlist(lapply(alignment, FUN = toString))),
      pattern = " ",
      replacement = ""
    ),
    ","
  )
  root_vector <- unlist(strsplit(root_sequence, split = ""))
  if (!all(root_vector %in% c("a", "c", "g", "t"))) {
    stop("'root_sequence' must be one character vector of lowercase nucleotides") # nolint long string
  }
  if (length(alignment_sequences[[1]]) != length(root_vector)) {
    stop("'root_sequence' must have the same length as each taxon's sequence length") # nolint long string
  }

  n_mutations <- 0
  for (i in 1:length(alignment)) {
    sequence_vector <- alignment_sequences[[i]]
    n_mutations <- n_mutations + sum(root_vector != sequence_vector)
  }
  n_mutations
}
