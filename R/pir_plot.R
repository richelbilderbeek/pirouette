#' Plot the error 'BEAST2' makes from a known phylogeny
#' @inheritParams default_params_doc
#' @return a \code{ggplot2} plot
#' @seealso
#' \itemize{
#'   \item Use \link{create_test_pir_run_output} to create a test output
#'     of \link{pir_run}.
#'   \item Use \link{pir_plot_from_file} to plot the errors after have
#'     being saved to a \code{.csv} file
#'   \item Use \link{pir_plots} to plot the output of multiple runs,
#'     for example, the output of \link{pir_runs}
#' }
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @examples
#' pir_out <- create_test_pir_run_output(
#'   add_twin = TRUE,
#'   add_best = TRUE
#' )
#' pir_plot(pir_out)
#' @export
pir_plot <- function(
  pir_out,
  verbose = FALSE
) {
  tree_and_model_labels <- get_pir_plot_tree_and_model_labels(pir_out)

  df_long <- convert_pir_out_to_long(pir_out, verbose = verbose)

  pirouette::pir_plot_from_long(
    df_long,
    tree_and_model_labels = tree_and_model_labels
  )
}
