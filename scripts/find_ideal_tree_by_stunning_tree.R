# Goal: find the ideal tree,
# that is, the tree that will give the lowest error

# Approach:
# * use the most stunning tree ever created in living memory
# * do a pirouette run, and measure its error
library(pirouette)


set.seed(314)

# Create an ideal tree
n_tips <- 5
ideal_tree <- create_stunning_tree()
testit::assert(ape::Ntip(ideal_tree) == n_tips)
ape::plot.phylo(ideal_tree)
# Differs, so I (@richelbilderbeek) lost faith in this approach

# Determine its error

