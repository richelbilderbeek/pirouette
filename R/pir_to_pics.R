#' Create all pictures created by the \link{pirouette} pipeline
#'
#' These are the files created:
#' \itemize{
#'   \item \code{true_tree.png} the true/given phylogeny
#'   \item \code{true_alignment.png} the alignment simulated from the
#'     true/given phylogeny
#'   \item \code{twin_tree.png} the twin tree [1]
#'   \item \code{twin_alignment.png} the alignment simulated from the
#'     twin phylogeny
#'   \item \code{true_posterior_gen.png}
#'     the phylogenies of the Bayesian posterior,
#'     using a generative inference model,
#'     based on the alignment based on the true tree
#'   \item \code{twin_posterior_gen.png}
#'     the phylogenies of the Bayesian posterior,
#'     using a generative inference model,
#'     based on the alignment based on the twin tree
#'   \item \code{true_error_histogram_gen.png}
#'     the errors between the Bayesian posterior
#'     and true/given tree,
#'     using a generative inference model,
#'     plotted as a histogram
#'   \item \code{twin_error_histogram_gen.png}
#'     the errors between the Bayesian posterior
#'     and twin tree,
#'     using a generative inference model,
#'     plotted as a histogram
#'   \item \code{true_error_violin_gen.png}
#'     the errors between the Bayesian posterior
#'     and true/given tree,
#'     using a generative inference model,
#'     plotted as a violin plot
#'   \item \code{twin_error_violin_gen.png}
#'     the errors between the Bayesian posterior
#'     and twin tree,
#'     using a generative inference model,
#'     plotted as a violin plot
#'   \item \code{true_posterior_best.png}
#'     the phylogenies of the Bayesian posterior,
#'     using the best candidate inference model,
#'     based on the alignment based on the true tree
#'   \item \code{twin_posterior_best.png}
#'     the phylogenies of the Bayesian posterior,
#'     using the best candidate inference model,
#'     based on the alignment based on the twin tree
#'   \item \code{true_error_histogram_best.png}
#'     the errors between the Bayesian posterior
#'     and true/given tree,
#'     using the best candidate inference model,
#'     plotted as a histogram
#'   \item \code{twin_error_histogram_best.png}
#'     the errors between the Bayesian posterior
#'     and twin tree,
#'     using the best candidate inference model,
#'     plotted as a histogram
#'   \item \code{true_error_violin_best.png}
#'     the errors between the Bayesian posterior
#'     and true/given tree,
#'     using the best candidate inference model,
#'     plotted as a violin plot
#'   \item \code{twin_error_violin_best.png}
#'     the errors between the Bayesian posterior
#'     and twin tree,
#'     using the best candidate inference model,
#'     plotted as a violin plot
#' }
#' Items marked [1] are created dependent on the setup.
#' @inheritParams default_params_doc
#' @return the names of all files created
#' @author Richel J.C. Bilderbeek
#' @export
pir_to_pics <- function(
  phylogeny,
  pir_params,
  folder = tempdir()
) {
  # Trees
  ggtree::ggtree(phylogeny) + ggtree::theme_tree2() + ggtree::geom_tiplab() +
    ggplot2::ggsave(file.path(folder, "true_tree.png"))
  if (!is_one_na(pir_params$twinning_params)) {
    ggtree::ggtree(
      ape::read.tree(pir_params$twinning_params$twin_tree_filename)
    ) + ggtree::theme_tree2() + ggtree::geom_tiplab() +
      ggplot2::ggsave(file.path(folder, "twin_tree.png"))
  }

  # Alignments
  png(
    filename = file.path(folder, "true_alignment.png"),
    width = 800,
    height = 300
  )
  ape::image.DNAbin(
    ape::read.FASTA(file = pir_params$alignment_params$fasta_filename),
    grid = TRUE,
    show.bases = FALSE,
    legend = FALSE,
    cex.lab = 2.0,
    cex.axis = 2.0
  )
  dev.off()

  if (!is_one_na(pir_params$twinning_params)) {
    png(
      filename = file.path(folder, "twin_alignment.png"),
      width = 800,
      height = 300
    )
    ape::image.DNAbin(
      ape::read.FASTA(file = pir_params$twinning_params$twin_alignment_filename),
      grid = TRUE,
      show.bases = FALSE,
      legend = FALSE,
      cex.lab = 2.0,
      cex.axis = 2.0
    )
    dev.off()
  }

  # Posteriors

  # True, gen
  png(
    filename = file.path(folder, "true_posterior_gen.png"),
    width = 1000, height = 800
  )
  babette::plot_densitree(
    phylos = tracerer::parse_beast_trees(pir_params$experiments[[1]]$beast2_options$output_trees_filenames),
    alpha = 0.01,
    # consensus = rev(LETTERS[1:6]),
    cex = 2.0,
    scaleX = TRUE,
    scale.bar = FALSE
  )
  dev.off()

  # Twin, gen
  if (!is_one_na(pir_params$twinning_params)) {
    png(
      filename = file.path(folder, "twin_posterior_gen.png"),
      width = 1000, height = 800
    )
    babette::plot_densitree(
      phylos = tracerer::parse_beast_trees(to_twin_filename(pir_params$experiments[[1]]$beast2_options$output_trees_filenames)),
      alpha = 0.01,
      consensus = rev(LETTERS[1:6]),
      cex = 2.0,
      scaleX = TRUE,
      scale.bar = FALSE
    )
    dev.off()
  }

  # True, best
  png(
    filename = file.path(folder, "true_posterior_best.png"),
    width = 1000, height = 800
  )
  babette::plot_densitree(
    phylos = tracerer::parse_beast_trees(pir_params$experiments[[2]]$beast2_options$output_trees_filenames),
    alpha = 0.01,
    consensus = rev(LETTERS[1:6]),
    cex = 2.0,
    scaleX = TRUE,
    scale.bar = FALSE
  )
  dev.off()


  # Twin, best
  if (!is_one_na(pir_params$twinning_params)) {
    png(
      filename = file.path(folder, "twin_posterior_best.png"),
      width = 1000, height = 800
    )
    babette::plot_densitree(
      phylos = tracerer::parse_beast_trees(to_twin_filename(pir_params$experiments[[2]]$beast2_options$output_trees_filenames)),
      alpha = 0.01,
      consensus = rev(LETTERS[1:6]),
      cex = 2.0,
      scaleX = TRUE,
      scale.bar = FALSE
    )
    dev.off()
  }

  # Hist
  # True, gen
  df_errors_gen <- data.frame(
    error = read.csv(pir_params$experiments[[1]]$errors_filename)$x
  )

  ggplot2::ggplot(
    df_errors_gen,
    ggplot2::aes(x = error)
  ) + ggplot2::geom_histogram(binwidth = 0.01) +
    ggplot2::ggsave(file.path(folder, "true_error_histogram_gen.png"))

  # Twin, gen
  if (!is_one_na(pir_params$twinning_params)) {
    df_errors_twin_gen <- data.frame(error = read.csv(to_twin_filename(pir_params$experiments[[1]]$errors_filename))$x)

    ggplot2::ggplot(
      df_errors_twin_gen,
      ggplot2::aes(x = error)
    ) + ggplot2::geom_histogram(binwidth = 0.01) +
    ggplot2::ggsave(file.path(folder, "twin_error_histogram_gen.png"))
  }

  # True, best
  df_errors_best <- data.frame(error = read.csv(pir_params$experiments[[2]]$errors_filename)$x)

  ggplot2::ggplot(
    df_errors_best,
    ggplot2::aes(x = error)
  ) + ggplot2::geom_histogram(binwidth = 0.01) +
    ggplot2::ggsave(file.path(folder, "true_error_histogram_best.png"))

  # Twin, best
  if (!is_one_na(pir_params$twinning_params)) {

    df_errors_twin_best <- data.frame(
      error = read.csv(
        to_twin_filename(pir_params$experiments[[2]]$errors_filename)
      )$x
    )

    ggplot2::ggplot(
      df_errors_twin_best,
      ggplot2::aes(x = error)
    ) + ggplot2::geom_histogram(binwidth = 0.01) +
    ggplot2::ggsave(file.path(folder, "twin_error_histogram_best.png"))
  }

  # Violin plots
  # True, gen
  df_errors_gen <- data.frame(
    error = read.csv(pir_params$experiments[[1]]$errors_filename)$x
  )

  ggplot2::ggplot(
    df_errors_gen,
    ggplot2::aes(x = "", y = error)
  ) + ggplot2::geom_violin() +
    ggplot2::xlab("") +
    ggplot2::scale_y_continuous(breaks = seq(0.0, 1.0, by = 0.02)) +
    ggplot2::ggsave(file.path(folder, "true_error_violin_gen.png"))

  # Twin, gen
  if (!is_one_na(pir_params$twinning_params)) {
    df_errors_twin_gen <- data.frame(
      error = read.csv(
        to_twin_filename(pir_params$experiments[[1]]$errors_filename)
      )$x
    )

    ggplot2::ggplot(
      df_errors_twin_gen,
      ggplot2::aes(x = "", y = error)
    ) + ggplot2::geom_violin() +
      ggplot2::xlab("") +
      ggplot2::scale_y_continuous(breaks = seq(0.0, 1.0, by = 0.02)) +
      ggplot2::ggsave(file.path(folder, "twin_error_violin_gen.png"))
  }

  # True, best
  ggplot2::ggplot(
    df_errors_best,
    ggplot2::aes(x = "", y = error)
  ) + ggplot2::geom_violin() +
    ggplot2::xlab("") +
    ggplot2::scale_y_continuous(breaks = seq(0.0, 1.0, by = 0.02)) +
    ggplot2::ggsave(file.path(folder, "true_error_violin_best.png"))

  # Twin, best
  if (!is_one_na(pir_params$twinning_params)) {

    df_errors_twin_best <- data.frame(
      error = read.csv(
        to_twin_filename(pir_params$experiments[[2]]$errors_filename)
      )$x
    )

    ggplot2::ggplot(
      df_errors_twin_best,
      ggplot2::aes(x = "", y = error)
    ) + ggplot2::geom_violin() +
      ggplot2::xlab("") +
      ggplot2::scale_y_continuous(breaks = seq(0.0, 1.0, by = 0.02)) +
      ggplot2::ggsave(file.path(folder, "twin_error_violin_best.png"))
  }
}
