#' Plot the error 'BEAST2' makes from a known phylogeny
#' from tidy data
#' @param df_long the output created by \code{\link{pir_run}} in the long form
#' @return a \code{ggplot2} plot
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @export
pir_plot_from_long <- function(df_long) {
  testthat::expect_false("inference_model" %in% names(df_long))
  testthat::expect_false("tree" %in% names(df_long))
  testthat::expect_false("inference_model_weight" %in% names(df_long))
  testthat::expect_false("site_model" %in% names(df_long))
  testthat::expect_false("clock_model" %in% names(df_long))
  testthat::expect_false("tree_prior" %in% names(df_long))

  testthat::expect_true("error_index" %in% names(df_long))
  testthat::expect_true("error_value" %in% names(df_long))
  testthat::expect_true("tree_and_model" %in% names(df_long))

   # The description, such as 'JC, RLN, BD'
  testthat::expect_true("model_setting" %in% names(df_long))

  df_long$inference_model <- forcats::fct_collapse(
    df_long$tree_and_model,
    generative = c("true_generative", "twin_generative"),
    candidate = c("true_candidate", "twin_candidate")
  )

  # Satisfy R CMD check
  tree <- NULL; rm(tree) # nolint, fixes warning: no visible binding for global variable
  error_value <- NULL; rm(error_value) # nolint, fixes warning: no visible binding for global variable
  inference_model <- NULL; rm(inference_model) # nolint, fixes warning: no visible binding for global variable
  quantile <- NULL; rm(quantile) # nolint, fixes warning: no visible binding for global variable
  ..y.. <- NULL; rm(..y..) # nolint, fixes warning: no visible binding for global variable
  model_setting <- NULL; rm(model_setting) # nolint, fixes warning: no visible binding for global variable
  tree_and_model <- NULL; rm(tree_and_model) # nolint, fixes warning: no visible binding for global variable
  median <- NULL; rm(median) # nolint, fixes warning: no visible binding for global variable
  ..density.. <- NULL; rm(..density..) # nolint, fixes warning: no visible binding for global variable

  theme <- pirouette::get_pir_plot_theme()

  ##### Legend labels #####
  tree_and_model_labels <- pirouette::get_pir_plot_tree_and_model_labels(
    df_long
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
