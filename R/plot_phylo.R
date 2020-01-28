#' Plot a phylogeny
#' @inheritParams default_params_doc
#' @return a fancy tree plot
#' @author Giovanni
#' @examples
#' phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
#' pirouette::plot_phylo(phylogeny)
#' @export
plot_phylo <- function(
  phylogeny
) {
ggtree::ggtree(
  phylogeny,
  size = 1.05,
  linetype = 7
) +
  ggtree::geom_tiplab(
    size = 5.5,
    hjust = - 0.4,
    align = T,
    linesize = 5,
    color = "firebrick4"
  ) +
  ggtree::geom_treescale(
    x = 0.5,
    y = 5.5,
    width = 2,
    linesize = 1.05,
    fontsize = 6,
    offset = 0.15,
    color = "firebrick4"
  )
}
