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

  # Satisfy R CMD check
  tree <- NULL; rm(tree) # nolint, fixes warning: no visible binding for global variable
  error_value <- NULL; rm(error_value) # nolint, fixes warning: no visible binding for global variable
  inference_model <- NULL; rm(inference_model) # nolint, fixes warning: no visible binding for global variable
  quantile <- NULL; rm(quantile) # nolint, fixes warning: no visible binding for global variable
  ..y.. <- NULL; rm(..y..) # nolint, fixes warning: no visible binding for global variable
  model_setting <- NULL; rm(model_setting) # nolint, fixes warning: no visible binding for global variable
  tree_and_model <- NULL; rm(tree_and_model) # nolint, fixes warning: no visible binding for global variable

  ##############################################################################
  # Data wrangling
  ##############################################################################
  # Convert to long form
  df <- pir_out
  first_col_index <- which(names(df) == "error_1")
  df_long <- tidyr::gather(
    df, "error_index", "error_value", first_col_index:ncol(df)
  )

  # Convert factor values to human-readable strings
  df_long$site_model <- plyr::revalue(
    df_long$site_model, c("JC69" = "JC", "TN93" = "TN"), warn_missing = FALSE)
  df_long$clock_model <- plyr::revalue(
    df_long$clock_model,
    c("strict" = "Strict", "relaxed_log_normal" = "RLN"), warn_missing = FALSE
  )
  df_long$tree_prior <- plyr::revalue(
    df_long$tree_prior,
    c(
      "yule" = "Yule",
      "birth_death" = "BD",
      "coalescent_bayesian_skyline" = "CBS",
      "coalescent_constant_population" = "CCP",
      "coalescent_exp_population" = "CEP"
    ),
    warn_missing = FALSE
  )

  # Add column tree_and_model, the combination of tree and model type
  df_long$tree_and_model <- interaction(
    df_long$tree,
    df_long$inference_model,
    sep = "_"
  )

  # Add model_setting, the combination of all inference models
  df_long$model_setting <- interaction(
    df_long$site_model,
    df_long$clock_model,
    df_long$tree_prior,
    sep = ", "
  )
  df_long <- df_long[order(df_long$tree), ]
  df_long$model_setting <-
    factor(df_long$model_setting, levels = unique(df_long$model_setting))
  df_long$inference_model <-
    factor(df_long$inference_model, levels = unique(df_long$inference_model))
  rownames(df_long) <- mapply(1:nrow(df_long), FUN = toString)

  ##############################################################################
  # Theme
  ##############################################################################
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
  )

  ##############################################################################
  # Legend labels
  ##############################################################################
  get_first <- function(x) utils::head(x, n = 1)
  # True, Generative
  tg_label <- NULL
  tg_model <- get_first(
    df_long$model_setting[df_long$tree_and_model == "true_generative"]
  )
  if (length(tg_model)) {
    tg_label <- paste("Generative, true:", tg_model)
  }
  # Twin, Generative
  wg_label <- NULL
  wg_model <- get_first(
    df_long$model_setting[df_long$tree_and_model == "twin_generative"]
  )
  if (length(wg_model)) {
    wg_label <- paste("Generative, twin:", wg_model)
  }
  # True, Best
  tb_label <- NULL
  tb_model <- get_first(
    df_long$model_setting[df_long$tree_and_model == "true_candidate"]
  )
  if (length(tb_model)) {
    tb_label <- paste("Best, true:", tb_model)
  }
  # Twin, Best
  wb_label <- NULL
  wb_model <- get_first(
    df_long$model_setting[df_long$tree_and_model == "twin_candidate"]
  )
  if (length(wb_model)) {
    wb_label <- paste("Best, twin:", wb_model)
  }

  # Collect all labels. Absent models have NULL labels and are thus ignored
  tree_and_model_labels <- c(
    tg_label,
    wg_label,
    tb_label,
    wb_label
  )

  ##############################################################################
  # Fill and line colors
  ##############################################################################

  # Line colors: must be darker than the fill color
  # Tree true has primary color, twin a lighter shade
  # Generative model is red, candidate blue
  tree_and_model_line_colors <- c(
    "true_generative" = "#FF0000", # Red
    "twin_generative" = "#FF8888", # Light red
    "true_candidate" = "#0000FF", # Blue
    "twin_candidate" = "#8888FF"  # Light blue
  )

  # Fill colors: must be lighter than the colors at the edges
  # Tree true has primary color, twin a lighter shade
  # Generative model is red, candidate blue
  tree_and_model_fill_colors <- c(
    "true_generative" = "#FF3333", # Red
    "twin_generative" = "#FFAAAA", # Light red
    "true_candidate" = "#3333FF", # Blue
    "twin_candidate" = "#AAAAFF"  # Light blue
  )

  ##############################################################################
  # Medians for the vertical lines
  ##############################################################################
  # Collect the medians
  medians <- df_long %>%
    dplyr::group_by(tree_and_model) %>%
    dplyr::summarise(median = stats::median(error_value))

  ##############################################################################
  # Plot it
  ##############################################################################
  ggplot2::ggplot(
    data = df_long,
    ggplot2::aes(
      x = error_value,
      color = tree_and_model,
      fill = tree_and_model
    )
  ) +
    ggplot2::geom_density() +
    ggplot2::scale_color_manual(
      values = tree_and_model_line_colors,
      labels = tree_and_model_labels
    ) +
    ggplot2::scale_fill_manual(
      values = tree_and_model_fill_colors,
      labels = tree_and_model_labels
    ) +
    ggplot2::scale_x_continuous(
      minor_breaks = seq(0.0, 1.0, 0.01)
    ) +
    ggplot2::geom_vline(
      data = medians,
      ggplot2::aes(
        xintercept = median,
        color = tree_and_model
      ),
      linetype = "dashed"
    ) +
    ggplot2::ggtitle("Inference error distribution") +
    ggplot2::labs(
    x = "Error",
    y = "Density",
    fill = "Model and tree",
    color = "Model and tree"
  ) + theme
}
