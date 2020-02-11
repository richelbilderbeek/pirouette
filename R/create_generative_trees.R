#' Create a Yule tree.
#'
#' A Yule model is also known as a pure-birth model;
#' a birth-death model without extinction.
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' n_taxa <- 31
#' crown_age <- 41
#'
#' phylogeny <- create_yule_tree(
#'   n_taxa = n_taxa,
#'   crown_age = crown_age
#' )
#'
#' expect_equal(n_taxa, ape::Ntip(phylogeny))
#' expect_equal(crown_age, beautier::get_crown_age(phylogeny))
#' @export
create_yule_tree <- function(
  n_taxa = 6,
  crown_age = 10,
  n_0 = 2
) {
  diff <- (log(n_taxa) - log(n_0)) / crown_age
  mu <- 0
  lambda <- diff + mu
  sim_tree <- TESS::tess.sim.taxa.age(
    n = 1,
    lambda = lambda,
    mu     = mu,
    nTaxa = n_taxa,
    age = crown_age,
    MRCA = TRUE
  )[[1]]
  sim_tree
}

#' Create a (constant-rate) birth-death (BD) tree
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' n_taxa <- 31
#' crown_age <- 41
#'
#' phylogeny <- create_bd_tree(
#'   n_taxa = n_taxa,
#'   crown_age = crown_age
#' )
#'
#' expect_equal(n_taxa, ape::Ntip(phylogeny))
#' expect_equal(crown_age, beautier::get_crown_age(phylogeny))
#' @export
create_bd_tree <- function(
  n_taxa = 6,
  crown_age = 10,
  n_0 = 2,
  mu = 0.1
) {
  diff <- (log(n_taxa) - log(n_0)) / crown_age
  lambda <- diff + mu
  sim_tree <- TESS::tess.sim.taxa.age(
    n = 1,
    lambda = lambda,
    mu     = mu,
    nTaxa = n_taxa,
    age = crown_age,
    MRCA = TRUE
  )[[1]]
  sim_tree
}

#' Create an exemplary diversity-dependent (DD) birth-death tree.
#'
#' Or: create a DD tree with a strong DD effect.
#'
#' This algorithm does so, by simulating \code{best_of_n_trees}
#' trees, then pick the first tree that has a gamma statistic
#' within the lowest 5 percent of all gammas. Trees with such
#' a low gamma, have the strongest DD effect, as these
#' deviate strongest from the expected exponential growth
#' that regular birth-death (BD) trees have.
#' @inheritParams default_params_doc
#' @param best_of_n_trees simulate this number of DD trees with
#' the desired number of taxa,
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' n_taxa <- 31
#' crown_age <- 41
#'
#' phylogeny <- create_dd_tree(
#'   n_taxa = n_taxa,
#'   crown_age = crown_age
#' )
#'
#' expect_equal(n_taxa, ape::Ntip(phylogeny))
#' expect_equal(crown_age, beautier::get_crown_age(phylogeny))
#' @export
create_dd_tree <- function(
  n_taxa = 6,
  crown_age = 10,
  n_0 = 2,
  extinction_rate = 0.1,
  best_of_n_trees = 100
) {
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

  sim_trees <- list()
  gammas <- rep(-1, best_of_n_trees)
  i <- 1
  while (i < best_of_n_trees) {

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
  # Pick the first tree that has a gamma within the lowest 5% of all gammas.
  # Why not the lowest?
  tree_id <- which(
    abs(gammas - stats::quantile(gammas, probs = c(0.05))) ==
      min(abs(gammas - stats::quantile(gammas, probs = c(0.05))))
  )
  sim_trees[[tree_id]]
}
