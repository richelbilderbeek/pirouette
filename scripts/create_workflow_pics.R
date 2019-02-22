# Creates the figures for the workflow image in the documentation
setwd("/home/richel/GitHubs/pirouette/doc")

library(pirouette)
library(ggplot2)

phylogeny <- ape::read.tree(text = "(((1:1,2:1):1, 3:2):1, 4:3);")

pir_params <- create_pir_params(
  alignment_params = create_alignment_params(
    root_sequence = pirouette::create_blocked_dna(length = 40),
    mutation_rate = 0.5 * 1.0 / 3.0
  ),
  twinning_params = create_twinning_params()
)
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
# nLTTs
################################################################################

png(filename = "nltt.png", width = 1000, height = 800)
nLTT::nltts_plot(
  tracerer::parse_beast_trees(pir_params$experiments[[1]]$beast2_options$output_trees_filenames),
  dt = 0.001,
  plot_nltts = TRUE
)
nLTT::nltts_plot(c(phylogeny), col = "red", lwd = 3, replot = TRUE)
dev.off()

png(filename = "nltt_twin.png", width = 1000, height = 800)
nLTT::nltts_plot(
  tracerer::parse_beast_trees(to_twin_filename(pir_params$experiments[[1]]$beast2_options$output_trees_filenames)),
  dt = 0.001,
  plot_nltts = TRUE
)
nLTT::nltts_plot(c(ape::read.tree(pir_params$twinning_params$twin_tree_filename)), col = "red", lwd = 3, replot = TRUE)
dev.off()


################################################################################
# histogram of errors
################################################################################

# STUB
ggplot2::ggplot(
  data.frame(errors = rnorm(n = 1000)),
  aes(x = errors)
) + geom_histogram() + ggsave("errors.png")

ggplot2::ggplot(
  data.frame(errors = rnorm(n = 1000)),
  aes(x = errors)
) + geom_histogram() + ggsave("errors_twin.png")

if (1 == 2) {
  ggplot2::ggplot(
    data.frame(errors = read.csv(pir_params$error_measure_params$errors_filename)),
    aes(x = errors)
  ) + geom_histogram()
}
