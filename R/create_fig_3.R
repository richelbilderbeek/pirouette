#' Create figure 3 for pirouette article
#' @inheritParams default_params_doc
#' @return a ggplot2 plot
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_fig_3 <- function() {
  phylogeny  <- ape::read.tree(text = "((A:4, B:4):1, (C:4, D:4) :1);")
  alignment_params <- create_alignment_params(
    root_sequence = create_blocked_dna(length = 1000),
    mutation_rate = 0.1
  )
  model_select_param <- create_best_model_select_param() # nolint pirouette function
  errors <- pir_run( # nolint pirouette function
    phylogeny = phylogeny,
    alignment_params = alignment_params,
    model_select_params = model_select_param
  )
  pir_plot(errors) # nolint pirouette function
}
