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
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
#'
#' pir_params <- create_test_pir_params(
#'   twinning_params = create_twinning_params()
#' )
#'
#' if (rappdirs::app_dir()$os != "win" &&
#'   is_on_ci() && is_beast2_installed()
#' ) {
#'
#'   pir_out <- pir_run(phylogeny = phylogeny, pir_params = pir_params)
#'
#'   folder <- tempdir()
#'   expected_filenames <- c(
#'     file.path(folder, "true_tree.png"),
#'     file.path(folder, "true_alignment.png"),
#'     file.path(folder, "twin_tree.png"),
#'     file.path(folder, "twin_alignment.png"),
#'     file.path(folder, "true_posterior_gen.png"),
#'     file.path(folder, "twin_posterior_gen.png"),
#'     file.path(folder, "true_error_histogram_gen.png"),
#'     file.path(folder, "twin_error_histogram_gen.png"),
#'     file.path(folder, "true_error_violin_gen.png"),
#'     file.path(folder, "twin_error_violin_gen.png")
#'   )
#'   expect_true(all(!file.exists(expected_filenames)))
#'
#'   created_filenames <- pir_to_pics(
#'     phylogeny = phylogeny,
#'     pir_params = pir_params,
#'     folder = folder
#'   )
#'
#'   expect_true(all(file.exists(expected_filenames)))
#' }
#' @export
pir_to_pics <- function(
  phylogeny,
  pir_params,
  consensus = rev(sort(phylogeny$tip.label)),
  folder = tempdir()
) {
  error <- NULL; rm(error) # nolint, fixes warning: no visible binding for global variable

  filenames <- NULL

  # Trees
  filename <- file.path(folder, "true_tree.png")
  grDevices::png(
    filename = filename,
    width = 1000, height = 800
  )
  ape::plot.phylo(phylogeny)
  ape::add.scale.bar()
  grDevices::dev.off()
  filenames <- filename

  # Alignments
  filename <- file.path(folder, "true_alignment.png")
  grDevices::png(
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
  grDevices::dev.off()
  filenames <- c(filenames, filename)

  # Posteriors
  first_experiment <- utils::head(pir_params$experiments, n = 1)[[1]]
  last_experiment <- utils::tail(pir_params$experiments, n = 1)[[1]]
  check_experiment(first_experiment) # nolint pirouette function
  check_experiment(last_experiment) # nolint pirouette function

  # True, gen
  if (first_experiment$inference_conditions$model_type == "generative") {
    filename <- file.path(folder, "true_posterior_gen.png")
    grDevices::png(
      filename = filename,
      width = 1000, height = 800
    )
    babette::plot_densitree(
      phylos = tracerer::parse_beast_trees(
        first_experiment$beast2_options$output_trees_filenames
      ),
      alpha = 0.01,
      consensus = consensus,
      cex = 2.0,
      scaleX = TRUE,
      scale.bar = FALSE
    )
    grDevices::dev.off()
    filenames <- c(filenames, filename)
  }
  # True, best
  if (last_experiment$inference_conditions$model_type == "candidate") {
    filename <- file.path(folder, "true_posterior_best.png")
    grDevices::png(
      filename = filename,
      width = 1000, height = 800
    )
    babette::plot_densitree(
      phylos = tracerer::parse_beast_trees(
        last_experiment$beast2_options$output_trees_filenames
      ),
      alpha = 0.01,
      consensus = consensus,
      cex = 2.0,
      scaleX = TRUE,
      scale.bar = FALSE
    )
    grDevices::dev.off()
    filenames <- c(filenames, filename)
  }
  # Hist
  # True, gen
  if (first_experiment$inference_conditions$model_type == "generative") {
    df_errors_gen <- data.frame(
      error = utils::read.csv(first_experiment$errors_filename)$x
    )

    filename <- file.path(folder, "true_error_histogram_gen.png")
    ggplot2::ggplot(
      df_errors_gen,
      ggplot2::aes(x = error)
    ) + ggplot2::geom_histogram(binwidth = 0.01) +
      ggplot2::ggsave(filename, width = 7, height = 7, units = "in")
    filenames <- c(filenames, filename)
  }

  # True, best
  if (last_experiment$inference_conditions$model_type == "candidate") {
    df_errors_best <- data.frame(
      error = utils::read.csv(last_experiment$errors_filename)$x
    )

    filename <- file.path(folder, "true_error_histogram_best.png")
    ggplot2::ggplot(
      df_errors_best,
      ggplot2::aes(x = error)
    ) + ggplot2::geom_histogram(binwidth = 0.01) +
      ggplot2::ggsave(filename, width = 7, height = 7, units = "in")
    filenames <- c(filenames, filename)
  }

  # Violin plots
  # True, gen
  if (first_experiment$inference_conditions$model_type == "generative") {
    df_errors_gen <- data.frame(
      error = utils::read.csv(first_experiment$errors_filename)$x
    )

    filename <- file.path(folder, "true_error_violin_gen.png")
    ggplot2::ggplot(
      df_errors_gen,
      ggplot2::aes(x = "", y = error)
    ) + ggplot2::geom_violin() +
      ggplot2::xlab("") +
      ggplot2::scale_y_continuous(breaks = seq(0.0, 1.0, by = 0.02)) +
      ggplot2::ggsave(filename, width = 7, height = 7, units = "in")
    filenames <- c(filenames, filename)
  }

  # True, best
  if (last_experiment$inference_conditions$model_type == "candidate") {
    df_errors_best <- data.frame(
      error = utils::read.csv(last_experiment$errors_filename)$x
    )

    filename <- file.path(folder, "true_error_violin_best.png")
    ggplot2::ggplot(
      df_errors_best,
      ggplot2::aes(x = "", y = error)
    ) + ggplot2::geom_violin() +
      ggplot2::xlab("") +
      ggplot2::scale_y_continuous(breaks = seq(0.0, 1.0, by = 0.02)) +
      ggplot2::ggsave(filename, width = 7, height = 7, units = "in")
    filenames <- c(filenames, filename)
  }

  if (!beautier::is_one_na(pir_params$twinning_params)) {
    twin_filenames <- pir_to_pics_twin( # nolint pirouette function
      pir_params = pir_params,
      consensus = consensus,
      folder = folder
    )
    filenames <- c(filenames, twin_filenames)
  }

  filenames
}

#' Create the twin pipeline pictures
#' @inheritParams default_params_doc
#' @return the names of all files created
#' @author Richèl J.C. Bilderbeek
#' @noRd
pir_to_pics_twin <- function(
  pir_params,
  consensus = rev(sort(ape::read.tree(
    pir_params$twinning_params$twin_tree_filename)$tip.label)),
  folder = tempdir()
) {
  testit::assert(!beautier::is_one_na(pir_params$twinning_params))

  error <- NULL; rm(error) # nolint, fixes warning: no visible binding for global variable

  filenames <- NULL

  # Trees
  filename <- file.path(folder, "twin_tree.png")
  grDevices::png(
    filename = filename,
    width = 1000, height = 800
  )
  ape::plot.phylo(ape::read.tree(pir_params$twinning_params$twin_tree_filename))
  ape::add.scale.bar()
  grDevices::dev.off()
  filenames <- filename

  # Alignment
  filename <- file.path(folder, "twin_alignment.png")
  grDevices::png(
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
  grDevices::dev.off()
  filenames <- c(filenames, filename)

  # Posteriors
  first_experiment <- utils::head(pir_params$experiments, n = 1)[[1]]
  last_experiment <- utils::tail(pir_params$experiments, n = 1)[[1]]
  check_experiment(first_experiment) # nolint pirouette function
  check_experiment(last_experiment) # nolint pirouette function

  # Twin, gen
  if (first_experiment$inference_conditions$model_type == "generative") {
    filename <- file.path(folder, "twin_posterior_gen.png")
    grDevices::png(
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
      consensus = consensus,
      cex = 2.0,
      scaleX = TRUE,
      scale.bar = FALSE
    )
    grDevices::dev.off()
    filenames <- c(filenames, filename)
  }

  # Twin, best
  filename <- file.path(folder, "twin_posterior_best.png")
  grDevices::png(
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
    consensus = consensus,
    cex = 2.0,
    scaleX = TRUE,
    scale.bar = FALSE
  )
  grDevices::dev.off()
  filenames <- c(filenames, filename)

  # Hist
  # Twin, gen
  if (first_experiment$inference_conditions$model_type == "generative") {
    beautier::check_file_exists(
      to_twin_filename(first_experiment$errors_filename)
    )

    df_errors_twin_gen <- data.frame(
      error = utils::read.csv(
        to_twin_filename(first_experiment$errors_filename)
      )$x
    )

    filename <- file.path(folder, "twin_error_histogram_gen.png")
    ggplot2::ggplot(
      df_errors_twin_gen,
      ggplot2::aes(x = error)
    ) + ggplot2::geom_histogram(binwidth = 0.01) +
      ggplot2::ggsave(filename, width = 7, height = 7, units = "in")
    filenames <- c(filenames, filename)
  }

  # Twin, best
  if (last_experiment$inference_conditions$model_type == "candidate") {
    df_errors_twin_best <- data.frame(
      error = utils::read.csv(
        to_twin_filename(last_experiment$errors_filename)
      )$x
    )

    filename <- file.path(folder, "twin_error_histogram_best.png")
    ggplot2::ggplot(
      df_errors_twin_best,
      ggplot2::aes(x = error)
    ) + ggplot2::geom_histogram(binwidth = 0.01) +
      ggplot2::ggsave(filename, width = 7, height = 7, units = "in")
    filenames <- c(filenames, filename)
  }

  # Violin plots
  # Twin, gen
  if (first_experiment$inference_conditions$model_type == "generative") {
    df_errors_twin_gen <- data.frame(
      error = utils::read.csv(
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
      ggplot2::ggsave(filename, width = 7, height = 7, units = "in")
    filenames <- c(filenames, filename)
  }

  # Twin, best
  if (last_experiment$inference_conditions$model_type == "candidate") {
    df_errors_twin_best <- data.frame(
      error = utils::read.csv(
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
        ggplot2::ggsave(filename, width = 7, height = 7, units = "in")
    filenames <- c(filenames, filename)
  }
  filenames
}
