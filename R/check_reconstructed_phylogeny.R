#' Check if the phylogeny is a reconstructed phylogeny.
#'
#' Will \link{stop} if there are extinct species in the phylogeny
#' @inheritParams default_params_doc
#' @export
check_reconstructed_phylogeny <- function(phylogeny) {
  if (!is.null(geiger::is.extinct(phylogeny))) {
    stop("A reconstructed phylogeny must not contain extinct species")
  }
}
