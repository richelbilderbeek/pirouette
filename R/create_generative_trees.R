#' Create a yule tree
#' @inheritParams default_params_doc
#' @export
#' @author Giovanni Laudanno
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
#' @export
#' @author Giovanni Laudanno
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
