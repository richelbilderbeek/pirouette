#' Plot the output of \link{pir_runs}.
#' @inheritParams default_params_doc
#' @return a \link{ggplot2} plot
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Create fake pir_run output
#' pir_outs <- list()
#' pir_outs[[1]] <- create_test_pir_run_output(
#'   add_twin = TRUE,
#'   add_best = TRUE
#' )
#' pir_outs[[2]] <- pir_outs[[1]]
#'
#' # Plot the (fake) output
#' expect_silent(pir_plots(pir_outs))
#' @export
pir_plots <- function(pir_outs) {

}
