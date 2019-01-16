#' Create a twin tree
#' @inheritParams default_params_doc
#' @export
#' @author Richel J.C. Bilderbeek
create_twin_tree <- function(phylogeny) {
  create_bd_tree(
    phylogeny = phylogeny,
    seed = 42
  )$bd_tree
}
