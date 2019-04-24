#' Create an "ideal" tree
#' @inheritParams default_params_doc
#' @param n_replicates number of trees simulated to pick the one with
#'   the highest likelihood of
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @examples
#'   library(testthat)
#'
#'   n_taxa <- 31
#'   crown_age <- 41
#'
#'   phylogeny <- create_ideal_tree(
#'     n_taxa = n_taxa,
#'     crown_age = crown_age
#'   )
#'
#'   expect_equal(n_taxa, ape::Ntip(phylogeny))
#'   expect_equal(crown_age, beautier::get_crown_age(phylogeny))
#' @export
create_ideal_tree <- function(
  n_taxa,
  crown_age,
  n_0 = 2,
  n_replicates = 1e4
) {
  lambda <- (log(n_taxa) - log(n_0)) / crown_age
  mu <- 0

  sim_trees <- TESS::tess.sim.taxa.age(
    n = n_replicates,
    lambda = lambda,
    mu     = mu,
    nTaxa = n_taxa,
    age = crown_age,
    MRCA = TRUE
  )
  liks <- rep(NA, n_replicates)
  for (n in 1:n_replicates) {
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
