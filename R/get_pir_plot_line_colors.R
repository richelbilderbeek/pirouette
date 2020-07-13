#' Internal function to get the line colors for \link{pir_plot}
#' @return a \code{ggplot2} plot
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @export
get_pir_plot_line_colors <- function() {
  c(
    "true_generative" = "#FF0000", # Red
    "twin_generative" = "#E77E22", # Orange
    "true_candidate" = "#0000FF", # Blue
    "twin_candidate" = "#229955"  # Green
  )
}
