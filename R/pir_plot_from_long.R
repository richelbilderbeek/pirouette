#' Plot the error 'BEAST2' makes from a known phylogeny
#' from tidy data
#' @inheritParams default_params_doc
#' @return a \code{ggplot2} plot
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @export
pir_plot_from_long <- function(
  df_long,
  tree_and_model_labels = get_tree_and_model_descriptions()
) {
  testthat::expect_equal(2, ncol(df_long))
  testthat::expect_true("error_value" %in% names(df_long))
  testthat::expect_true("tree_and_model" %in% names(df_long))

  # Either 'generative' or 'best'
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

  ##### Legend labels #####
  tree_and_model_labels <- tree_and_model_labels[
    tree_and_model_labels$tree_and_model %in% df_long$tree_and_model,
  ]$description


  ##### Fill and line colors #####
  medians <- df_long %>%
    dplyr::group_by(tree_and_model) %>%
    dplyr::summarise(median = stats::median(error_value), .groups = "keep")
  testthat::expect_true("tree_and_model" %in% names(medians))
  testthat::expect_true("median" %in% names(medians))

  index <- trunc(0.95 * length(df_long$error_value))
  x_top <- sort(df_long$error_value)[index]

  alpha <- 0.5

  # Only generative
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
        bins = 30,
        alpha = alpha,
        position = "identity"
      )
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
      bins = 30,
      alpha = alpha,
      position = "identity"
    ) +
      ggplot2::facet_wrap(
        .~ inference_model,
        ncol = 1,
        labeller = ggplot2::labeller(inference_model = inference_model_labels)
      )
  }
  plot + ggplot2::scale_color_manual(
    values = pirouette::get_pir_plot_line_colors(),
    labels = tree_and_model_labels
  ) +
  ggplot2::scale_fill_manual(
    values = pirouette::get_pir_plot_fill_colors(),
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
  ) + ggplot2::ggtitle("Inference error distribution") +
    ggplot2::labs(
    x = "Error",
    y = "Density",
    fill = "Model and tree",
    color = "Model and tree"
  ) + pirouette::get_pir_plot_theme()
}
