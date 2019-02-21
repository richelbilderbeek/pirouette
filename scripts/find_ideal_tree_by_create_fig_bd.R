# Goal: find the ideal tree,
# that is, the tree that will give the lowest error

# Approach:
# * use the most stunning tree ever created in living memory
# * do a pirouette run, and measure its error
library(pirouette)
source("/home/richel/GitHubs/pirouette_article/scripts/create_fig_bd.R")


set.seed(314)

# Create an ideal tree
n_tips <- 3
ideal_tree <- create_fig_bd(
  t_0 = 10.0,
  n_taxa = 5,
  n_0 = 2
)$tree
testit::assert(ape::Ntip(ideal_tree) == n_tips)
ape::plot.phylo(ideal_tree)
# Differs, so I (@richelbilderbeek) lost faith in this approach

# Determine its error

