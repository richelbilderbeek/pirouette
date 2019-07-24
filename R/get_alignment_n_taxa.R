#' Get the number of taxa of an alignment
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @export
#' @export
get_alignment_n_taxa <- function(alignment) {
  testit::assert(class(alignment) == "DNAbin")
  length(alignment)
}
