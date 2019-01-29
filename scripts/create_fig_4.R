#' Create figure 4 for pirouette article
#' @inheritParams default_params_doc
#' @return a ggplot2 plot
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_fig_4 <- function() {
  phylogeny <- ape::read.tree(text = "((A:4, B:4):1, (C:4, D:4) :1);")
  alignment_params <- create_alignment_params(
    root_sequence = create_blocked_dna(length = 1000),
    mutation_rate = 0.1
  )
  model_select_param <- create_best_model_select_param() # nolint pirouette function
  twinning_params <- create_twinning_params() # nolint pirouette function
  errors  <- pir_run( # nolint pirouette function
    phylogeny = phylogeny,
    alignment_params = alignment_params,
    model_select_params = model_select_param,
    twinning_params = twinning_params
  )
  pir_plot(errors) # nolint pirouette function
}

#' Create figure 4 file
#' @inheritParams default_params_doc
#' @return the name of the file the plot is saved to
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_fig_4_file <- function(
  fig_4_filename = "figure_4.png"
) {
  # create figure
  fig_4 <- create_fig_4() # nolint internal function

  # save output
  ggplot2::ggsave(
    filename = fig_4_filename,
    plot = fig_4
  )
  fig_4_filename
}

# Run
library(pirouette)
create_fig_4_file()