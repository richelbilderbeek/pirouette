#' Create a twin tree
#' #@export
#' @author Richel J.C. Bilderbeek
create_twin_tree <- function(phylogeny) {
  create_bd_tree(
    parameters = list(lambda = 0.1, mu = 0.01, seed = 42),
    mbd_tree = phylogeny,
    mbd_l_matrix = dododo::phylo2L(phylogeny)
  )$bd_tree
}
