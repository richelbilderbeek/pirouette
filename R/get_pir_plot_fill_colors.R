#' Internal function to get the fill colors for \link{pir_plot}
#' @return a \code{ggplot2} plot
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @export
get_pir_plot_fill_colors <- function() {
  # Fill colors: must be lighter than the colors at the edges
  c(
    "true_generative" = "#FF3333", # Red
    "twin_generative" = "#F99F55", # Orange
    "true_candidate" = "#3333FF", # Blue
    "twin_candidate" = "#559988"  # Green
  )
}
