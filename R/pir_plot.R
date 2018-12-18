#' Plot the error BEAST2 make from the known phylogeny
#' @param pir_out the output created by \code{\link{pir_run}}
#' @return a ggplot2 plot
#' @author Richel J.C. Bilderbeek
#' @export
pir_plot <- function(pir_out) {

  df <- pir_out
  first_col_index <- which(names(df) == "error_1")
  df_long <- tidyr::gather(df, "error_index", "error_value", first_col_index:ncol(df))

  ggplot2::ggplot(
    data = df_long,
    ggplot2::aes(x = tree, y = error_value, fill = inference_model)
  ) + ggplot2::geom_violin() +
    ggplot2::ggtitle("The error BEAST2 makes")

}
