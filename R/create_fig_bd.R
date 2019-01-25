#' Create figure bd for pirouette article
#' @inheritParams default_params_doc
#' @return a ggplot2 plot
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_fig_bd <- function() {
  phylogeny <- ape::read.tree(text = "((A:4, B:4):1, (C:4, D:4) :1);")
  i <- 31 #or 24
  {
    set.seed(i)
    phylogeny <- DDD::dd_sim(pars = c(0.09, 0, Inf), age = 10, ddmodel = 1)$tes
    figure_bd <- ggtree::ggtree(phylogeny) + ggtree::theme_tree2(); figure_bd
  }
  figure_bd
}
