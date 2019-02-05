#' Create a twin tree
#' @inheritParams default_params_doc
#' @export
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
create_twin_tree <- function(
  phylogeny,
  twin_model = "bd",
  method
) {
  if (!(twin_model %in% get_twin_models())) {
   stop("This twin model is not implemented")
  }
  if (twin_model == "bd") {
    twin_tree <- create_bd_tree(
      phylogeny = phylogeny,
      seed = 42
    )$tree
  }
  if (twin_model == "yule") {
    twin_tree <- create_yule_tree(
      phylogeny = phylogeny,
      seed = 42
    )$tree
  }
  testit::assert(beautier::is_phylo(twin_tree))
  twin_tree
}
