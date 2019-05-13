#' Count mutations
#' @inheritParams default_params_doc
#' @return the number of mutations
#' @author Giovanni Laudanno
#' @examples
#' library(testthat)
#'
#' # it works
#' alignment <- ape::as.DNAbin(
#'   x = list(species_1 = strsplit("aaaa", split = "")[[1]])
#' )
#' expect_equal(count_n_mutations(alignment, "acgt"), 3)
#'
#' @export
count_n_mutations <- function(
  alignment,
  root_sequence
) {

  if (class(alignment) != "DNAbin") {
    stop("'alignment' must be of class 'ape::DNAbin'")
  }

  if (is.matrix(alignment)) {
    alignment_sequences <- matrix(
      unname(unlist(as.character(alignment))),
      nrow = length(labels(alignment)),
      ncol = ncol(alignment),
      byrow = TRUE
    )
  }
  if (is.list(alignment)) {
    alignment_sequences <- matrix(
      unname(unlist(as.character(alignment))),
      nrow = length(labels(alignment)),
      ncol = length(as.character(alignment)[[1]]),
      byrow = TRUE
    )
  }

  root_vector <- unlist(strsplit(root_sequence, split = ""))
  if (!all(root_vector %in% c("a", "c", "g", "t"))) {
    stop("'root_sequence' must be one character vector of lowercase nucleotides") # nolint long string
  }
  if (ncol(alignment_sequences) != length(root_vector)) {
    stop("'root_sequence' must have the same length as each taxon's sequence length") # nolint long string
  }

  n_mutation <- 0
  for (i in 1:nrow(alignment_sequences)) {
    sequence_vector <- alignment_sequences[i, ]
    n_mutation <- n_mutation + sum(root_vector != sequence_vector)
  }
  n_mutation
}
