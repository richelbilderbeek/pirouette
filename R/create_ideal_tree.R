#' Create an "ideal" tree
#' @inheritParams default_params_doc
#' @export
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
create_ideal_tree <- function(
  n_taxa,
  crown_age,
  n_0 = 2
) {
  lambda <- (log(n_taxa) - log(n_0)) / crown_age
  mu <- 0
  n_replicas <- 1e4

  sim_trees <- TESS::tess.sim.taxa.age(
    n = n_replicas,
    lambda = lambda,
    mu     = mu,
    nTaxa = n_taxa,
    age = crown_age,
    MRCA = TRUE
  )
  liks <- rep(NA, n_replicas)
  for (n in 1:n_replicas) {
    liks[n] <-  DDD::bd_loglik(
      pars1 = c(lambda, 0, mu, 0),
      pars2 = c(0, 3, 0, 0, n_0 - 1),
      brts = ape::branching.times(sim_trees[[n]]),
      missnumspec = 0
    )
  }
  best_n <- which(liks == max(liks))
  ideal_tree <- sim_trees[[best_n]]

  testit::assert(beautier::is_phylo(ideal_tree))
  ideal_tree
}
