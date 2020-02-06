#' Create a Yule tree
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#'   library(testthat)
#'
#'   n_taxa <- 31
#'   crown_age <- 41
#'
#'   phylogeny <- create_yule_tree(
#'     n_taxa = n_taxa,
#'     crown_age = crown_age
#'   )
#'
#'   expect_equal(n_taxa, ape::Ntip(phylogeny))
#'   expect_equal(crown_age, beautier::get_crown_age(phylogeny))
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

#' Create a bd tree
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#'   library(testthat)
#'
#'   n_taxa <- 31
#'   crown_age <- 41
#'
#'   phylogeny <- create_bd_tree(
#'     n_taxa = n_taxa,
#'     crown_age = crown_age
#'   )
#'
#'   expect_equal(n_taxa, ape::Ntip(phylogeny))
#'   expect_equal(crown_age, beautier::get_crown_age(phylogeny))
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

#' Create a dd tree
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#'   library(testthat)
#'
#'   n_taxa <- 31
#'   crown_age <- 41
#'
#'   phylogeny <- create_dd_tree(
#'     n_taxa = n_taxa,
#'     crown_age = crown_age
#'   )
#'
#'   expect_equal(n_taxa, ape::Ntip(phylogeny))
#'   expect_equal(crown_age, beautier::get_crown_age(phylogeny))
#' @export
create_dd_tree <- function(
  n_taxa = 6,
  crown_age = 10,
  n_0 = 2,
  mu = 0.1
) {
  if (n_0 != 2) {
    stop("This works only for 2 starting species")
  }
  diff <- (log(n_taxa) - log(n_0)) / crown_age
  lambda <- 3 * (diff + mu)
  kk <- n_taxa

  n_trees <- 100
  sim_trees <- vector("list", n_trees)
  gammas <- rep(-1, n_trees)
  i <- 1
  while (i < n_trees) {

    sim <- DDD::dd_sim(
      pars = c(lambda, mu, kk),
      age = crown_age,
      ddmodel = 1
    )
    if (ape::Ntip(sim$tes) == n_taxa) {
      sim_trees[[i]] <- sim$tes
      gammas[i] <- phytools::gammatest(
        phytools::ltt(sim$tes, plot = FALSE, gamma = FALSE)
      )$gamma
      i <- i + 1
    }
  }
  tree_id <- which(
    abs(gammas - quantile(gammas, probs = c(0.05))) ==
      min(abs(gammas - quantile(gammas, probs = c(0.05))))
  )
  sim_tree <- sim_trees[[tree_id]]
  sim_tree
}
