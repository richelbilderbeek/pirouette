# Creates the figures for the workflow image in the documentation
setwd("/home/richel/GitHubs/pirouette/doc")

library(pirouette)
library(ggplot2)
library(ggthemes)
set.seed(314)

phylogeny  <- ape::read.tree(
  text = "(((((A:2, B:2):2, C:4):2, D:6):2, E:8):2, F:10);"
)

pir_params <- create_pir_params(
  alignment_params = create_alignment_params(
    root_sequence = pirouette::create_blocked_dna(length = 40),
    mutation_rate = 0.5 * 1.0 / 3.0
  ),
  twinning_params = create_twinning_params()
)

################################################################################
# Settings to run on Peregrine cluster
################################################################################
root_folder <- path.expand("~/GitHubs/pirouette/doc")
pir_params$alignment_params$fasta_filename <- file.path(root_folder, "alignment.fasta"))
pir_params$experiments[[1]]$beast2_options$input_filename <- file.path(root_folder, "beast2_input.xml")
pir_params$experiments[[1]]$beast2_options$output_log_filename <- file.path(root_folder, "beast2_output.log")
pir_params$experiments[[1]]$beast2_options$output_trees_filenames <- file.path(root_folder, "beast2_output.trees")
pir_params$experiments[[1]]$beast2_options$output_state_filename <- file.path(root_folder, "beast2_output.xml.state")
pir_params$experiments[[1]]$errors_filename <- file.path(root_folder, "error.csv")
################################################################################

errors <- pir_run(phylogeny = phylogeny, pir_params = pir_params)

################################################################################
# trees
################################################################################

png(filename = "phylogeny.png", width = 400, height = 300)
ape::plot.phylo(phylogeny, cex = 2.0, edge.width = 2.0)
dev.off()

png(filename = "phylogeny_twin.png", width = 400, height = 300)
ape::plot.phylo(ape::read.tree(pir_params$twinning_params$twin_tree_filename), cex = 2.0, edge.width = 2.0)
dev.off()

################################################################################
# alignment
################################################################################

png(filename = "alignment.png", width = 800, height = 300)
ape::image.DNAbin(
  ape::read.FASTA(file = pir_params$alignment_params$fasta_filename),
  grid = TRUE,
  show.bases = FALSE,
  legend = FALSE,
  cex.lab = 2.0,
  cex.axis = 2.0
)
dev.off()

png(filename = "alignment_twin.png", width = 800, height = 300)
ape::image.DNAbin(
  ape::read.FASTA(file = pir_params$twinning_params$twin_alignment_filename),
  grid = TRUE,
  show.bases = FALSE,
  legend = FALSE,
  cex.lab = 2.0,
  cex.axis = 2.0
)
dev.off()

################################################################################
# posteriors
################################################################################

png(filename = "densitree.png", width = 1000, height = 800)
babette::plot_densitree(
  phylos = tracerer::parse_beast_trees(pir_params$experiments[[1]]$beast2_options$output_trees_filenames),
  alpha = 0.01,
  consensus = as.character(c(1:4)),
  cex = 2.0,
  scaleX = TRUE,
  scale.bar = FALSE
)
dev.off()

png(filename = "densitree_twin.png", width = 1000, height = 800)
babette::plot_densitree(
  phylos = tracerer::parse_beast_trees(to_twin_filename(pir_params$experiments[[1]]$beast2_options$output_trees_filenames)),
  alpha = 0.01,
  consensus = as.character(c(1:4)),
  cex = 2.0,
  scaleX = TRUE,
  scale.bar = FALSE
)
dev.off()

################################################################################
# histogram of errors
################################################################################

df_errors <- data.frame(error = read.csv(pir_params$experiments[[1]]$errors_filename)$x)
df_errors_twin <- data.frame(error = read.csv(to_twin_filename(pir_params$experiments[[1]]$errors_filename))$x)

ggplot2::ggplot(
  df_errors,
  aes(x = error)
) + geom_histogram(binwidth = 0.01) + ggsave("errors.png")

ggplot2::ggplot(
  df_errors_twin,
  aes(x = error)
) + geom_histogram(binwidth = 0.01) + ggsave("errors_twin.png")

ggplot2::ggplot(
  df_errors,
  aes(x = "", y = error)
) + geom_violin() +
  xlab("") +
  scale_y_continuous(breaks = seq(0.0, 1.0, by = 0.02)) +
  ggsave("errors_violin.png")

ggplot2::ggplot(
  df_errors_twin,
  aes(x = "", y = error)
) + geom_violin() +
  xlab("") +
  scale_y_continuous(breaks = seq(0.0, 1.0, by = 0.02)) +
  ggsave("errors_violin_twin.png")
