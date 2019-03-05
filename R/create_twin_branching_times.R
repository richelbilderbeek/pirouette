#' Generate twin branching times given estimated lambda and mu and the original
#'   tree
#' @inheritParams default_params_doc
#' @return twin branching times
#' @author Giovanni Laudanno
#' @export
create_twin_branching_times <- function(
  seed,
  lambda,
  mu,
  phylogeny,
  n_replicas,
  method
) {
  # Retrieve info from the phylogeny
  age  <- beautier::get_crown_age(phylogeny)
  phylo_brts <- sort(
    convert_tree2brts(phylogeny), # nolint pirouette function
    decreasing = TRUE
  )
  n_tips <- ape::Ntip(phylogeny)
  soc <- 1 + n_tips - length(phylo_brts)

  # Simulate branching times
  set.seed(seed)
  max_n <- 1 * (method == "random_tree") +
     n_replicas *  (method == "max_clade_cred" | method == "max_likelihood")
  sim_trees <- TESS::tess.sim.taxa.age(
    n = max_n,
    lambda = lambda,
    mu     = mu,
    nTaxa = n_tips,
    age = age,
    MRCA = TRUE
  )
  if (method == "max_likelihood") {
    liks <- rep(NA, max_n)
    for (n in 1:max_n) {
      liks[n] <-  DDD::bd_loglik(
        pars1 = c(lambda, 0, mu, 0),
        pars2 = c(0, 3, 0, 0, soc - 1),
        brts = ape::branching.times(sim_trees[[n]]),
        missnumspec = 0
      )
    }
    best_n <- which(liks == max(liks))
    tree0 <- sim_trees[[best_n]]
  }
  if (method == "max_clade_cred" | method == "random_tree") {
    tree0 <- phangorn::maxCladeCred(sim_trees)
  }
  brts0 <- convert_tree2brts(tree0) # nolint pirouette function
  brts0
}
