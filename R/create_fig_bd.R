#' Create figure bd for pirouette article
#' @inheritParams default_params_doc
#' @return a ggplot2 plot
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_fig_bd <- function() {
  lambda <- 0.07
  mu <- 0
  max_seed <- 1e5
  phylogenies <- TESS::tess.sim.taxa.age(
    n = max_seed,
    age = 10,
    nTaxa = 6,
    lambda = lambda,
    mu = mu,
    MRCA = TRUE
  )
  cons_phylo <- phangorn::maxCladeCred(phylogenies)
  figure_bd <- ggtree::ggtree(cons_phylo) + ggtree::theme_tree2()
  figure_bd
}
