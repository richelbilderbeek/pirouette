# Goal: find the ideal tree,
# that is, the tree that will give the lowest error

# Approach:
# * simulate a random BD tree
# * do a pirouette run, keep it iff it has the lowest sum of errors

n_tips <- 3
sequence_length <- 100
burn_in_fraction <- 0.2
n_repeats <- 3

sim_bd_tree <- function(n_taxa) {
  lambda <- runif(n = 1, min = 0.5, max = 1.5) * 1.0 / n_taxa
  mu <- runif(n = 1, min = 0.1, max = 0.3) * lambda
  TESS::tess.sim.taxa.age(
    n = 1,
    lambda = lambda,
    mu     = mu,
    nTaxa = n_taxa,
    age = 1.0,
    MRCA = TRUE
  )[[1]]
}


lowest_sum_errors <- Inf
best_phylogeny <- sim_bd_tree(n_tips)

set.seed(314)


for (i in seq(1, n_repeats)) {
  message(paste0("Progress: ", i, "/", n_repeats))
  # Create a random tree
  candidate_tree <- sim_bd_tree(n_tips)
  testthat::expect_true(ape::Ntip(candidate_tree) == n_tips)
  ape::plot.phylo(candidate_tree)

  # Do a pirouette run and select the best
  pir_params <- create_pir_params(
    alignment_params = create_alignment_params(
      root_sequence = create_blocked_dna(sequence_length)
    ),
    experiments = list(
      create_experiment(
        inference_model = create_inference_model(
          mcmc = create_mcmc(chain_length = 1000000, store_every = 1000)
        )
      )
    )
  )

  errors <- pirouette::pir_run(
    phylogeny = candidate_tree,
    pir_params = pir_params
  )
  first_error_col <- which(colnames(errors) == "error_1")
  last_error_col <- ncol(errors)
  n_errors <- 1 + last_error_col - first_error_col
  all_errors <- as.numeric(errors[1, first_error_col:last_error_col])
  clean_errors <- tracerer::remove_burn_in(all_errors, burn_in_fraction = burn_in_fraction)
  sum_errors <- sum(clean_errors)

  if (sum_errors < lowest_sum_errors) {
    lowest_sum_errors <- sum_errors
    best_phylogeny <- candidate_tree
    message(paste("Better tree found with sum errors:", sum_errors))
    ape::plot.phylo(best_phylogeny)
  }
}

