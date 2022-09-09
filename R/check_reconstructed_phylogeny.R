#' Check if the phylogeny is a reconstructed phylogeny.
#'
#' Will \link{stop} if there are extinct species in the phylogeny
#' @inheritParams default_params_doc
#' @return nothing
#' @export
check_reconstructed_phylogeny <- function(phylogeny) {
  l_table <- DDD::phylo2L(phylogeny)
  no_extinct <- all(l_table[, 4] == -1)
  if (!no_extinct) {
    stop("A reconstructed phylogeny must not contain extinct species")
  }
  invisible(phylogeny)
}
