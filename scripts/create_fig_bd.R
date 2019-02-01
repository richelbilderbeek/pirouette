#' Create figure bd for pirouette article
#' @inheritParams default_params_doc
#' @return a ggplot2 plot
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_fig_bd <- function() {
  phylogeny <- ape::read.tree(text = "((A:4, B:4):1, (C:4, D:4) :1);")
  i <- 31 #or 24
  set.seed(i)
  phylogeny <- DDD::dd_sim(pars = c(0.09, 0, Inf), age = 10, ddmodel = 1)$tes
  figure_bd <- ggtree::ggtree(phylogeny) + ggtree::theme_tree2(); figure_bd
  figure_bd
}

#' Create figure bd file
#' @inheritParams default_params_doc
#' @return the name of the file the plot is saved to
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_fig_bd_file <- function(
  fig_bd_filename = "figure_bd.png"
) {
  # create figure
  fig_bd <- create_fig_bd() # nolint internal function

  # save output
  ggplot2::ggsave(
    filename = fig_bd_filename,
    plot = fig_bd
  )
  fig_bd_filename
}

# Run
library(pirouette)
create_fig_bd_file()
