#' Plot the error 'BEAST2' makes from a known phylogeny
#' from tidy data
#' @inheritParams default_params_doc
#' @return a \code{ggplot2} plot
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @export
pir_plot_from_long <- function(
  tree_and_model_errors,
  tree_and_model_descriptions = get_tree_and_model_descriptions()
) {
  pirouette::check_tree_and_model_errors(tree_and_model_errors)


  # Either 'generative' or 'best'
  # Suppress warning message:
  # Unknown levels in `f`: twin_generative, true_candidate, twin_candidate
  # which happens, for example, when tree_and_model_errors$tree_and_model
  # contains only true_generative
  suppressWarnings(
    tree_and_model_errors$inference_model <- forcats::fct_collapse(
      tree_and_model_errors$tree_and_model,
      generative = c("true_generative", "twin_generative"),
      candidate = c("true_candidate", "twin_candidate")
    )
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
  tree_and_model_labels <- tree_and_model_descriptions[
    tree_and_model_descriptions$tree_and_model %in% tree_and_model_errors$tree_and_model, # nolint indeed a long line
  ]$description


  ##### Fill and line colors #####
  medians <- tree_and_model_errors %>%
    dplyr::group_by(tree_and_model) %>%
    dplyr::summarise(median = stats::median(error_value), .groups = "keep")
  testthat::expect_true("tree_and_model" %in% names(medians))
  testthat::expect_true("median" %in% names(medians))

  # Prevent the long tail
  x_top <- stats::quantile(
    x = tree_and_model_errors$error_value,
    probs = 0.95
  )
  binwidth <- x_top / 30

  alpha <- 0.5



  # Only generative
  if (length(unique(tree_and_model_errors$inference_model)) == 1) {

    plot <- ggplot2::ggplot(
      data = tree_and_model_errors,
      ggplot2::aes(
        x = error_value,
        color = tree_and_model,
        fill = tree_and_model
      )
    ) +
      ggplot2::geom_histogram(
        data = tree_and_model_errors,
        binwidth = binwidth,
        alpha = alpha,
        position = "identity"
      )
  } else {

    testthat::expect_true(
      length(unique(tree_and_model_errors$inference_model)) > 1
    )

    medians$inference_model <- forcats::fct_collapse(
      medians$tree_and_model,
      generative = c("true_generative", "twin_generative"),
      candidate = c("true_candidate", "twin_candidate")
    )
    tree_and_model_errors$inference_model <- plyr::revalue(
      tree_and_model_errors$inference_model,
      c(
        "generative" = "Generative",
        "candidate" = "Best"
      ),
      warn_missing = FALSE
    )
    medians$inference_model <- plyr::revalue(
      medians$inference_model,
      c(
        "generative" = "Generative",
        "candidate" = "Best"
      ),
      warn_missing = FALSE
    )
    inference_model_labels <- c("Generative", "Best")
    names(inference_model_labels) <- c("Generative", "Best")

    ##### Plot it (Double plot) #####

    plot <- ggplot2::ggplot(
      data = tree_and_model_errors,
      ggplot2::aes(
        x = error_value,
        color = tree_and_model,
        fill = tree_and_model
      )
    ) +
      ggplot2::geom_histogram(
      binwidth = binwidth,
      alpha = alpha,
      position = "identity"
    ) +
      ggplot2::facet_wrap(
        . ~ inference_model,
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
  ggplot2::coord_cartesian(xlim = c(0.0, x_top)) +
  ggplot2::geom_vline(
    data = medians,
    ggplot2::aes(
      xintercept = median,
      color = tree_and_model
    ),
    linetype = "dashed"
  ) +
    ggplot2::labs(
    x = "Error",
    y = "Density",
    fill = "Model and tree",
    color = "Model and tree"
  ) + pirouette::get_pir_plot_theme()
}
