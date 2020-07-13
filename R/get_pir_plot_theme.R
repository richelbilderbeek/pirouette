#' Get the \link{pir_plot} theme
#' @return the \link{pir_plot} theme
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @export
get_pir_plot_theme <- function() {
  label_size <- 13
  label_face <- "italic"
  title_size <- 18
  title_face <- "bold"
  ticks_size <- 12
  ticks_face <- "plain"
  ticks_color <- "black"
  theme_title <- ggplot2::element_text(
    hjust = 0.5, face = title_face, size = title_size
  )
  theme_major_label <- ggplot2::element_text(
    face = label_face, size = label_size
  )
  theme_minor_label <- ggplot2::element_text(
    face = ticks_face, color = ticks_color, size = ticks_size
  )
  theme <- ggplot2::theme(
    plot.title = theme_title,
    axis.title.x = theme_major_label,
    axis.title.y = theme_major_label,
    legend.title = theme_major_label,
    axis.text.x = theme_minor_label,
    axis.text.y = theme_minor_label,
    legend.text = theme_minor_label,
    strip.text.x = ggplot2::element_text(size = 12)
  ) + ggplot2::theme_bw()
  theme
}
