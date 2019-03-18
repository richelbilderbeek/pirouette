#' Create a twin tree
#' @inheritParams default_params_doc
#' @export
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
create_twin_tree <- function(
  phylogeny,
  twinning_params = create_twinning_params()
) {
  if (!(twinning_params$twin_model %in% get_twin_models())) {
    stop("This twin model is not implemented")
  }
  if (twinning_params$twin_model == "bd") {
    twin_tree <- twin_to_bd_tree(
      phylogeny = phylogeny,
      twinning_params = twinning_params
    )$tree
  }
  if (twinning_params$twin_model == "yule") {
    twin_tree <- twin_to_yule_tree(
      phylogeny = phylogeny,
      twinning_params = twinning_params
    )$tree
  }
  testit::assert(beautier::is_phylo(twin_tree))
  testit::assert(
    all.equal(
      max(ape::branching.times(phylogeny)),
      max(ape::branching.times(twin_tree))
    )
  )
  twin_tree
}
