#' Create an exemplary diversity-dependent (DD) birth-death tree.
#'
#' Or: create a DD tree with a strong DD effect.
#'
#' This algorithm does so, by simulating \code{best_of_n_trees}
#' trees, then picks the tree that has the gamma statistic furthest
#' away from zero.
#' Trees with such a gamma statistic, have the strongest DD effect, as these
#' deviate strongest from the expected exponential growth
#' that regular birth-death (BD) trees have.
#' @inheritParams default_params_doc
#' @param best_of_n_trees simulate this number of DD trees with
#' the desired number of taxa,
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' n_taxa <- 3
#' crown_age <- 1
#'
#' phylogeny <- create_exemplary_dd_tree(
#'   n_taxa = n_taxa,
#'   crown_age = crown_age
#' )
#' @export
create_exemplary_dd_tree_giappo <- function(#nolint indeed a long function name
  n_taxa = 6,
  crown_age = 10,
  extinction_rate = 0.1,
  best_of_n_trees = 100
) {
  # n_0: initial number of species
  n_0 <- 2
  testthat::expect_gte(n_taxa, 2)
  testthat::expect_gt(crown_age, 0.0)
  testthat::expect_equal(n_0, 2)
  testthat::expect_gte(extinction_rate, 0.0)
  testthat::expect_gte(best_of_n_trees, 1)
  # Pick parameters as such that the tree reaches carrying capacity
  # lambda: speciation rate
  # kk: carrying capacity
  mu <- extinction_rate
  diff <- (log(n_taxa) - log(n_0)) / crown_age
  lambda <- 3 * (diff + mu)
  kk <- n_taxa

  if (n_taxa == 2) {
    sim_tree <- TESS::tess.sim.taxa.age(
      n = 1,
      nTaxa = n_taxa,
      age = crown_age,
      lambda = lambda,
      mu = mu
    )[[1]]
    return(sim_tree)
  }

  sim_trees <- list()
  gammas <- rep(-1, best_of_n_trees)
  i <- 1
  while (i <= best_of_n_trees) {

    sim <- DDD::dd_sim(
      pars = c(lambda, mu, kk),
      age = crown_age,
      ddmodel = 1 # linear dependence in speciation rate with parameter K
    )
    if (ape::Ntip(sim$tes) == n_taxa) {
      sim_trees[[i]] <- sim$tes
      gammas[i] <- phytools::gammatest(
        phytools::ltt(sim$tes, plot = FALSE, gamma = FALSE)
      )$gamma
      i <- i + 1
    }
  }
  # Pick the tree that has the lowest gamma
  tree_id <- which(abs(gammas) == max(abs(gammas)))
  sim_trees[[tree_id]]
}
