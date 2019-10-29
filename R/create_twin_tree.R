#' Create a twin tree
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @examples
#' phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
#' twin_phylogeny <- create_twin_tree(phylogeny)
#'
#' library(testthat)
#' # Twin is a phylogeny
#' expect_true(is_phylo(twin_phylogeny))
#'
#' # Twin tree has the same number of taxa as the original tree
#' expect_equal(ape::Ntip(phylogeny), ape::Ntip(twin_phylogeny))
#'
#' # Twin tree has the same crown age as the original tree
#' expect_equal(
#'   max(ape::branching.times(phylogeny)),
#'   max(ape::branching.times(twin_phylogeny))
#' )
#' @export
create_twin_tree <- function(
  phylogeny,
  twinning_params = create_twinning_params()
) {
  beautier::check_phylogeny(phylogeny)
  pirouette::check_twinning_params(twinning_params)
  if (twinning_params$method == "newskool") {
    twin_tree <- twinning_params$sim_twin_tree_function(phylogeny)
  } else {
    testit::assert(!"deprecatedz")
    if (twinning_params$twin_model == "birth_death") {
      twin_tree <- twin_to_bd_tree_obsolete(
        phylogeny = phylogeny,
        twinning_params = twinning_params
      )
    }
    else if (twinning_params$twin_model == "yule") {
      twin_tree <- twin_to_yule_tree_obsolete(
        phylogeny = phylogeny,
        twinning_params = twinning_params
      )
    } else {
      testit::assert(twinning_params$twin_model == "copy_true")
      twin_tree <- phylogeny
    }
  }
  testit::assert(beautier::is_phylo(twin_tree))
  testit::assert(ape::Ntip(phylogeny) == ape::Ntip(twin_tree))
  testit::assert(
    all.equal(
      max(ape::branching.times(phylogeny)),
      max(ape::branching.times(twin_tree))
    )
  )
  twin_tree
}
