#' Create figure bd for pirouette article
#' @inheritParams default_params_doc
#' @return a ggplot2 plot
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_fig_bd <- function(
  phylogeny,
  twinning_params = create_twinning_params()
) {
  twin_bd_tree <- create_twin_tree(
    phylogeny = phylogeny,
    twinning_params = twinning_params
  )
  figure_bd <- ggtree::ggtree(twin_bd_tree) + ggtree::theme_tree2()
  figure_bd
}
