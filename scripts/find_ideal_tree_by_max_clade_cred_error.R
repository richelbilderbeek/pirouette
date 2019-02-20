# Goal: find the ideal tree, 
# that is, the tree that will give the lowest error

set.seed(314)

# Create an ideal tree
n_tips <- 3
ideal_tree <- ape::drop.fossil(phangorn::maxCladeCred(ape::rcoal(n = n_tips)))
testit::assert(ape::Ntip(ideal_tree) == n_tips)
ape::plot.phylo(ideal_tree)
# Differs, so I (@richelbilderbeek) lost faith in this approach

# Determine its error
