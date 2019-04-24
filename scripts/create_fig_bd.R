# Create figure bd file
#' @inheritParams default_params_doc
#' @return the name of the file the plot is saved to
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_fig_bd_file <- function(
  fig_bd_filename = "figure_bd.png"
) {
  phylogeny <- ape::read.tree(text = "((A:4, B:4):1, (C:4, D:4) :1);")
  i <- 31 #or 24
  set.seed(i)
  phylogeny <- DDD::dd_sim(pars = c(0.09, 0, Inf), age = 10, ddmodel = 1)$tes

    grDevices::png(
    filename = fig_bd_filename,
    width = 1000, height = 800
  )
  ape::plot.phylo(phylogeny)
  ape::add.scale.bar()
  grDevices::dev.off()
  fig_bd_filename
}

# Run
library(pirouette)
create_fig_bd_file()
