#' Plot the error BEAST2 make from the known phylogeny
#' @param pir_out the output created by \code{\link{pir_run}}
#' @return a ggplot2 plot
#' @seealso
#'   Use \link{create_test_pir_run_output} to create a test output
#'   of \link{pir_run}
#' @author Richèl J.C. Bilderbeek
#' @examples
#' pir_out <- create_test_pir_run_output(
#'   add_twin = TRUE,
#'   add_best = TRUE
#' )
#' pir_plot(pir_out)
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
  quantile <- NULL; rm(quantile) # nolint, fixes warning: no visible binding for global variable
  ..y.. <- NULL; rm(..y..) # nolint, fixes warning: no visible binding for global variable
  model_setting <- NULL; rm(model_setting) # nolint, fixes warning: no visible binding for global variable

  # Plot options
  label_size <- 13
  label_face <- "italic"
  title_size <- 18
  title_face <- "bold"
  ticks_size <- 12
  ticks_face <- "plain"
  ticks_color <- "black"

  df_long$model_setting <- interaction(
    df_long$site_model,
    df_long$clock_model,
    df_long$tree_prior,
    sep = "\n"
  )
  df_long <- df_long[order(df_long$tree), ]
  df_long$model_setting <-
    factor(df_long$model_setting, levels = unique(df_long$model_setting))
  df_long$inference_model <-
    factor(df_long$inference_model, levels = unique(df_long$inference_model))
  rownames(df_long) <- mapply(1:nrow(df_long), FUN = toString)

  ggplot2::ggplot(
    data = df_long,
    ggplot2::aes(
      x = model_setting,
      y = error_value,
      fill = tree
    )
  ) + ggplot2::geom_violin() +
    ggplot2::ggtitle("Inference error distribution") +
    ggplot2::labs(
      x = "Tree types",
      y = "Errors",
      fill = "Tree type"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = title_face, size = title_size), # nolint
      axis.title.x = ggplot2::element_text(face = label_face, size = label_size), # nolint
      axis.title.y = ggplot2::element_text(face = label_face, size = label_size), # nolint
      legend.title = ggplot2::element_text(face = label_face, size = label_size), # nolint
      axis.text.x = ggplot2::element_text(face = ticks_face, color = ticks_color, size = ticks_size), # nolint
      axis.text.y = ggplot2::element_text(face = ticks_face, color = ticks_color, size = ticks_size), # nolint
      legend.text = ggplot2::element_text(face = ticks_face, color = ticks_color, size = ticks_size), # nolint
      strip.text.x = ggplot2::element_text(size = 12)
    ) +
    ggplot2::xlab(
      "Inference model (Site model, Clock model, Tree prior)"
    ) +
    ggplot2::facet_grid(
      scale = "free", # Show only those categories that have values
      . ~ inference_model,
      labeller = ggplot2::labeller(
        inference_model = c
        (
          generative = "Generative",
          best = "Best Candidate"
        )
      )
    ) + ggplot2::scale_x_discrete(drop = TRUE)

}
