#' Plot the error BEAST2 make from the known phylogeny
#' @param pir_out the output created by \code{\link{pir_run}}
#' @return a ggplot2 plot
#' @author Richel J.C. Bilderbeek
#' @export
pir_plot <- function(pir_out) {

  df <- pir_out
  first_col_index <- which(names(df) == "error_1")
  df_long <- tidyr::gather(
    df, "error_index", "error_value", first_col_index:ncol(df)
  )

  # Satisfy R CMD check
  tree <- NULL; rm(tree) # nolint, fixes warning: no visible binding for global variable
  error_value <- NULL; rm(error_value) # nolint, fixes warning: no visible binding for global variable
  inference_model <- NULL; rm(inference_model) # nolint, fixes warning: no visible binding for global variable

  ggplot2::ggplot(
    data = df_long,
    ggplot2::aes(x = tree, y = error_value, fill = inference_model)
  ) + ggplot2::geom_violin() +
    ggplot2::geom_boxplot(width = 0.1, fill = "white") +
    ggplot2::stat_summary(
      geom = "label",
      fun.y = quantile,
      ggplot2::aes(label = sprintf("%1.3f", ..y..)),
      position = ggplot2::position_nudge(x = 0.1), size = 3.5,
      color = "black",
      fill = "white"
    ) +
    ggplot2::ggtitle("The error made by BEAST2")

}
