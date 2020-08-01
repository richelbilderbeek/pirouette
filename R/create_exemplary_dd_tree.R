#' Create an exemplary diversity-dependent (DD) birth-death tree.
#'
#' Create an exemplary diversity-dependent (DD) birth-death tree,
#' that is, a DD tree with a strong DD effect.
#' The DD tree produced most likely has the desired number
#' of taxa, but this is not always the case.
#'
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @seealso Use \link{create_exemplary_dd_tree_giappo}
#' for a different way to generate exemplary trees with
#' a strong DD effect.
#' @examples
#' create_exemplary_dd_tree(
#'   n_taxa = 3,
#'   crown_age = 1
#' )
#' @export
create_exemplary_dd_tree <- function(
  n_taxa = 6,
  crown_age = 10,
  extinction_rate = 0.1
) {
  n_0 <- 2
  testthat::expect_gte(n_taxa, 2)
  testthat::expect_gt(crown_age, 0.0)
  testthat::expect_equal(n_0, 2)
  testthat::expect_gte(extinction_rate, 0.0)
    diff <- (log(n_taxa) - log(n_0)) / crown_age
  speciation_rate <- 3.0 * (diff + extinction_rate)
  carrying_capacity <- n_taxa # clade-level
  dd_parameters <- c(speciation_rate, extinction_rate, carrying_capacity)
  ddmodel <- 1 # linear dependence in speciation rate with parameter K
  dd_sim_result <- DDD::dd_sim(
    pars = dd_parameters,
    age  = crown_age,
    ddmodel = ddmodel
  )
  phylogeny <- dd_sim_result$tes # Only extant species
  phylogeny
}
