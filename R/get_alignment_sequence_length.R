#' Get the sequence length of an alignment
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @export
get_alignment_sequence_length <- function(alignment) {
  testit::assert(class(alignment) == "DNAbin")
  length(alignment[[1]])
}
