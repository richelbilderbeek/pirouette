#' Obtain the crown age of a phylony
#' @inheritParams default_params_doc
#' @return the age of the phylogeny
#' @examples
#'   phylogeny <- ape::read.tree(text = "((A:1, B:1):1, (C:1, D:1):1);")
#'   crown_age <- get_crown_age(phylogeny)
#'   testit::assert(crown_age == 2.0)
#' @author Richel J.C. Bilderbeek
#' @export
get_crown_age <- function(
  phylogeny
) {
  if (class(phylogeny) != "phylo") {
    stop(
      "'phylogeny' must be of class 'phylo'"
    )
  }
  n_taxa <- length(phylogeny$tip.label)
  testit::assert(n_taxa > 0)
  ape::dist.nodes(phylogeny)[n_taxa + 1][1]
}
