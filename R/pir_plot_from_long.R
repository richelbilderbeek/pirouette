#' Plot the error 'BEAST2' makes from a known phylogeny
#' from tidy data
#' @param df_long the output created by \code{\link{pir_run}} in the long form
#' @return a \code{ggplot2} plot
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @export
pir_plot_from_long <- function(df_long) {
  ##### Theme #####
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
  ) +
    ggplot2::theme_bw()

  ##### Legend labels #####
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

  ##### Fill and line colors #####

  # Line colors: must be darker than the fill color
  tree_and_model_line_colors <- c(
    "true_generative" = "#FF0000", # Red
    "twin_generative" = "#E77E22", # Orange
    "true_candidate" = "#0000FF", # Blue
    "twin_candidate" = "#229955"  # Green
  )

  # Fill colors: must be lighter than the colors at the edges
  tree_and_model_fill_colors <- c(
    "true_generative" = "#FF3333", # Red
    "twin_generative" = "#F99F55", # Orange
    "true_candidate" = "#3333FF", # Blue
    "twin_candidate" = "#559988"  # Green
  )

  ##### Medians for the vertical lines #####

  # Collect the medians
  medians <- df_long %>%
    dplyr::group_by(tree_and_model) %>%
    dplyr::summarise(median = stats::median(error_value))

  ##### Only keep 95% of x axis values #####

  index <- trunc(0.95 * length(df_long$error_value))
  x_top <- sort(df_long$error_value)[index]

  ##### More aesthetic settings for the plots #####

  n_errors <- length(unique(df_long$error_index))
  bindwidth <- 0.1 / sqrt(n_errors)
  alpha <- 0.5

  ##### Plot it (Single Plot) #####

  if (length(unique(df_long$inference_model)) == 1) {

    plot <- ggplot2::ggplot(
      data = df_long,
      ggplot2::aes(
        x = error_value,
        color = tree_and_model,
        fill = tree_and_model
      )
    ) +
      ggplot2::geom_histogram(
        data = df_long,
        ggplot2::aes(y = bindwidth * ..density..), # nolint the dots in ..density.. are not improper ways to separate words here
        binwidth = bindwidth,
        alpha = alpha,
        position = "identity"
      ) +
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
      ggplot2::coord_cartesian(
        xlim = c(min(df_long$error_value), x_top)
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
  ##### Split Candidate plot from Generative Plot #####

  if (length(unique(df_long$inference_model)) > 1) {
    medians$inference_model <- gsub(
      x = gsub(
        x = medians$tree_and_model,
        pattern = "true_",
        replacement = ""
      ),
      pattern = "twin_",
      replacement = ""
    )
    df_long$inference_model <- plyr::revalue(
      df_long$inference_model,
      c(
        "candidate" = "Best",
        "generative" = "Generative"
      ),
      warn_missing = FALSE
    )
    medians$inference_model <- plyr::revalue(
      medians$inference_model,
      c(
        "candidate" = "Best",
        "generative" = "Generative"
      ),
      warn_missing = FALSE
    )
    inference_model_labels <- c("Best", "Generative")
    names(inference_model_labels) <- c("Generative", "Best")

    ##### Plot it (Double plot) #####

    plot <- ggplot2::ggplot(
      data = df_long,
      ggplot2::aes(
        x = error_value,
        color = tree_and_model,
        fill = tree_and_model
      )
    ) +
      ggplot2::geom_histogram(
      data = df_long,
      ggplot2::aes(y = bindwidth * ..density..), # nolint the dots in ..density.. are not improper ways to separate words here
      binwidth = bindwidth,
      alpha = alpha,
      position = "identity"
    ) +
      ggplot2::facet_wrap(
        .~ inference_model,
        ncol = 1,
        labeller = ggplot2::labeller(inference_model = inference_model_labels)
      ) +
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
      ggplot2::coord_cartesian(
        xlim = c(min(df_long$error_value), x_top)
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
  plot
}
