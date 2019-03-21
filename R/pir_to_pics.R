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
#' @param folder folder where the files are stored in.
#'   By default, this is a temporary folder
#' @return the names of all files created
#' @author Richel J.C. Bilderbeek
#' @export
pir_to_pics <- function(
  phylogeny,
  pir_params,
  folder = tempdir()
) {
  filenames <- NULL

  # Trees
  filename <- file.path(folder, "true_tree.png")
  ggtree::ggtree(phylogeny) + ggtree::theme_tree2() + ggtree::geom_tiplab() +
    ggplot2::ggsave(filename)
  filenames <- filename

  # Alignments
  filename <- file.path(folder, "true_alignment.png")
  png(
    filename = filename,
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
  filenames <- c(filenames, filename)

  # Posteriors
  first_experiment <- head(pir_params$experiments, n = 1)[[1]]
  last_experiment <- tail(pir_params$experiments, n = 1)[[1]]
  check_experiment(first_experiment)
  check_experiment(last_experiment)

  # True, gen
  if (first_experiment$inference_conditions$model_type == "generative") {
    filename <- file.path(folder, "true_posterior_gen.png")
    png(
      filename = filename,
      width = 1000, height = 800
    )
    babette::plot_densitree(
      phylos = tracerer::parse_beast_trees(
        first_experiment$beast2_options$output_trees_filenames
      ),
      alpha = 0.01,
      # consensus = rev(LETTERS[1:6]),
      cex = 2.0,
      scaleX = TRUE,
      scale.bar = FALSE
    )
    dev.off()
    filenames <- c(filenames, filename)
  }
  # True, best
  if (last_experiment$inference_conditions$model_type == "candidate") {
    filename <- file.path(folder, "true_posterior_best.png")
    png(
      filename = filename,
      width = 1000, height = 800
    )
    babette::plot_densitree(
      phylos = tracerer::parse_beast_trees(
        last_experiment$beast2_options$output_trees_filenames
      ),
      alpha = 0.01,
      consensus = rev(LETTERS[1:6]),
      cex = 2.0,
      scaleX = TRUE,
      scale.bar = FALSE
    )
    dev.off()
    filenames <- c(filenames, filename)
  }
  # Hist
  # True, gen
  if (first_experiment$inference_conditions$model_type == "generative") {
    df_errors_gen <- data.frame(
      error = read.csv(first_experiment$errors_filename)$x
    )

    filename <- file.path(folder, "true_error_histogram_gen.png")
    ggplot2::ggplot(
      df_errors_gen,
      ggplot2::aes(x = error)
    ) + ggplot2::geom_histogram(binwidth = 0.01) +
      ggplot2::ggsave(filename)
    filenames <- c(filenames, filename)
  }

  # True, best
  if (last_experiment$inference_conditions$model_type == "candidate") {
    df_errors_best <- data.frame(
      error = read.csv(last_experiment$errors_filename)$x
    )

    filename <- file.path(folder, "true_error_histogram_best.png")
    ggplot2::ggplot(
      df_errors_best,
      ggplot2::aes(x = error)
    ) + ggplot2::geom_histogram(binwidth = 0.01) +
      ggplot2::ggsave(filename)
    filenames <- c(filenames, filename)
  }

  # Violin plots
  # True, gen
  if (first_experiment$inference_conditions$model_type == "generative") {
    df_errors_gen <- data.frame(
      error = read.csv(first_experiment$errors_filename)$x
    )

    filename <- file.path(folder, "true_error_violin_gen.png")
    ggplot2::ggplot(
      df_errors_gen,
      ggplot2::aes(x = "", y = error)
    ) + ggplot2::geom_violin() +
      ggplot2::xlab("") +
      ggplot2::scale_y_continuous(breaks = seq(0.0, 1.0, by = 0.02)) +
      ggplot2::ggsave(filename)
    filenames <- c(filenames, filename)
  }

  # True, best
  if (last_experiment$inference_conditions$model_type == "candidate") {
    df_errors_best <- data.frame(
      error = read.csv(last_experiment$errors_filename)$x
    )

    filename <- file.path(folder, "true_error_violin_best.png")
    ggplot2::ggplot(
      df_errors_best,
      ggplot2::aes(x = "", y = error)
    ) + ggplot2::geom_violin() +
      ggplot2::xlab("") +
      ggplot2::scale_y_continuous(breaks = seq(0.0, 1.0, by = 0.02)) +
      ggplot2::ggsave(filename)
    filenames <- c(filenames, filename)
  }

  if (!is_one_na(pir_params$twinning_params)) {
    twin_filenames <- pir_to_pics_twin( # nolint pirouette function
      pir_params = pir_params,
      folder = folder
    )
    filenames <- c(filenames, twin_filenames)
  }

  filenames
}

#' Create the twin pipeline pictures
#' @inheritParams default_params_doc
#' @return the names of all files created
#' @author Richel J.C. Bilderbeek
#' @noRd
pir_to_pics_twin <- function(
  pir_params,
  folder = tempdir()
) {
  testit::assert(!is_one_na(pir_params$twinning_params))

  filenames <- NULL

  # Trees
  filename <- file.path(folder, "twin_tree.png")
  ggtree::ggtree(
    ape::read.tree(pir_params$twinning_params$twin_tree_filename)
  ) + ggtree::theme_tree2() + ggtree::geom_tiplab() +
    ggplot2::ggsave(filename)
  filenames <- filename

  # Alignment
  filename <- file.path(folder, "twin_alignment.png")
  png(
    filename = filename,
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
  filenames <- c(filenames, filename)

  # Posteriors
  first_experiment <- head(pir_params$experiments, n = 1)[[1]]
  last_experiment <- tail(pir_params$experiments, n = 1)[[1]]
  check_experiment(first_experiment)
  check_experiment(last_experiment)

  # Twin, gen
  if (first_experiment$inference_conditions$model_type == "generative") {
    filename <- file.path(folder, "twin_posterior_gen.png")
    png(
      filename = filename,
      width = 1000, height = 800
    )
    babette::plot_densitree(
      phylos = tracerer::parse_beast_trees(
        to_twin_filename(
          first_experiment$beast2_options$output_trees_filenames
        )
      ),
      alpha = 0.01,
      consensus = rev(LETTERS[1:6]),
      cex = 2.0,
      scaleX = TRUE,
      scale.bar = FALSE
    )
    dev.off()
    filenames <- c(filenames, filename)
  }

  # Twin, best
  filename <- file.path(folder, "twin_posterior_best.png")
  png(
    filename = filename,
    width = 1000, height = 800
  )
  babette::plot_densitree(
    phylos = tracerer::parse_beast_trees(
      to_twin_filename(
        last_experiment$beast2_options$output_trees_filenames
      )
    ),
    alpha = 0.01,
    consensus = rev(LETTERS[1:6]),
    cex = 2.0,
    scaleX = TRUE,
    scale.bar = FALSE
  )
  dev.off()
  filenames <- c(filenames, filename)

  # Hist
  # Twin, gen
  if (first_experiment$inference_conditions$model_type == "generative") {
    df_errors_twin_gen <- data.frame(
      error = read.csv(
        to_twin_filename(first_experiment$errors_filename)
      )$x
    )

    filename <- file.path(folder, "twin_error_histogram_gen.png")
    ggplot2::ggplot(
      df_errors_twin_gen,
      ggplot2::aes(x = error)
    ) + ggplot2::geom_histogram(binwidth = 0.01) + ggplot2::ggsave(filename)
    filenames <- c(filenames, filename)
  }

  # Twin, best
  if (last_experiment$inference_conditions$model_type == "candidate") {
    df_errors_twin_best <- data.frame(
      error = read.csv(
        to_twin_filename(last_experiment$errors_filename)
      )$x
    )

    filename <- file.path(folder, "twin_error_histogram_best.png")
    ggplot2::ggplot(
      df_errors_twin_best,
      ggplot2::aes(x = error)
    ) + ggplot2::geom_histogram(binwidth = 0.01) +
    ggplot2::ggsave(filename)
    filenames <- c(filenames, filename)
  }

  # Violin plots
  # Twin, gen
  if (first_experiment$inference_conditions$model_type == "generative") {
    df_errors_twin_gen <- data.frame(
      error = read.csv(
        to_twin_filename(first_experiment$errors_filename)
      )$x
    )

    filename <- file.path(folder, "twin_error_violin_gen.png")
    ggplot2::ggplot(
      df_errors_twin_gen,
      ggplot2::aes(x = "", y = error)
    ) + ggplot2::geom_violin() +
      ggplot2::xlab("") +
      ggplot2::scale_y_continuous(breaks = seq(0.0, 1.0, by = 0.02)) +
      ggplot2::ggsave(filename)
    filenames <- c(filenames, filename)
  }

  # Twin, best
  if (last_experiment$inference_conditions$model_type == "candidate") {
    df_errors_twin_best <- data.frame(
      error = read.csv(
        to_twin_filename(last_experiment$errors_filename)
      )$x
    )

    filename <- file.path(folder, "twin_error_violin_best.png")
    ggplot2::ggplot(
      df_errors_twin_best,
      ggplot2::aes(x = "", y = error)
    ) + ggplot2::geom_violin() +
      ggplot2::xlab("") +
      ggplot2::scale_y_continuous(breaks = seq(0.0, 1.0, by = 0.02)) +
      ggplot2::ggsave(filename)
    filenames <- c(filenames, filename)
  }
  filenames
}
