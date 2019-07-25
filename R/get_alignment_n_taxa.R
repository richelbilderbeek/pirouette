#' Get the number of taxa of an alignment
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @export
get_alignment_n_taxa <- function(alignment, verbose = FALSE) {
  if (class(alignment) != "DNAbin") {
    stop("'alignment' must be of class 'ape::DNAbin'")
  }
  length(labels(alignment))
}
