#' Convert a phylogeny to a twin Coalescent Bayesian
#' Skyline tree
#' @inheritParams default_params_doc
#' @return a CBS tree
#' @author Richel J.C Bilderbeek
#' @export
twin_to_cbs_tree <- function(
  phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
  winning_params = create_twinning_params()
) {
  # STUB
  twin_to_yule_tree(
    phylogeny = phylogeny,
    twinning_params = twinning_params
  )
}
