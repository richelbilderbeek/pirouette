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
#' expect_equal(count_n_mutation(alignment, "acgt"), 3)
#'
#' @export
count_n_mutation <- function(
  alignment,
  root_sequence
) {

  if (class(alignment) != "DNAbin") {
    stop("'alignment' must be of class 'ape::DNAbin'")
  }
  alignment_sequences <- strsplit(gsub(
    unname(unlist(lapply(alignment, FUN = toString))),
    pattern = " ",
    replacement = ""
  ), "," )
  root_vector <- unlist(strsplit(root_sequence, split = ""))
  if (!all(root_vector %in% c("a", "c", "g", "t"))) {
    stop("'root_sequence' must be one character vector of lowercase nucleotides") # nolint long string
  }
  if (length(alignment_sequences[[1]]) != length(root_vector)) {
    stop("'root_sequence' must have the same length as each taxon's sequence length") # nolint long string
  }

  n_mutation <- 0
  for (i in 1:length(alignment)) {
    sequence_vector <- alignment_sequences[[i]]
    n_mutation <- n_mutation + sum(root_vector != sequence_vector)
  }
  n_mutation
}
