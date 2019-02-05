#' Create an BD twin tree from a phylogeny
#' and save it as a file
#' @inheritParams default_params_doc
#' @return a twin BD tree of class \code{phylo},
#'   obtained from the corresponding phylogeny.
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_bd_tree <- function(
  phylogeny,
  twinning_params
) {
  seed <- twinning_params$rng_seed
  method <- twinning_params$method
  n_replicas <- twinning_params$n_replicas

  age  <- beautier::get_crown_age(phylogeny)
  phylo_brts <- sort(
    convert_tree2brts(phylogeny), # nolint pirouette function
    decreasing = TRUE
  )
  n_tips <- ape::Ntip(phylogeny)
  soc <- 1 + n_tips - length(phylo_brts)
  testit::assert(soc == 1 | soc == 2)
  difference <- log(n_tips) / age
  mu <- 0.1
  lambda <- mu + difference

  if (rappdirs::app_dir()$os != "win") {
    sink(file.path(rappdirs::user_cache_dir(), "ddd"))
  } else {
    sink(rappdirs::user_cache_dir())
  }
  # TODO: Issue #52: check the quality of inference of lambda and mu provided by bd_ML # nolint
  bd_pars <- DDD::bd_ML(
    brts = sort(phylo_brts, decreasing = TRUE),
    cond = 1, #conditioning on stem or crown age # nolint
    initparsopt = c(lambda, mu),
    idparsopt = 1:2,
    missnumspec = 0,
    tdmodel = 0,
    btorph = 1,
    soc = soc
  )
  sink()

  lambda_bd <- as.numeric(unname(bd_pars[1]))
  mu_bd <- as.numeric(unname(bd_pars[2]))
  testit::assert(!is.null(lambda_bd))
  testit::assert(is.numeric(lambda_bd))
  testit::assert(!is.null(mu_bd))
  testit::assert(is.numeric(mu_bd))

  # generate bd branching times from the inferred parameters
  set.seed(seed)
  max_n <- 1 * (method == "random_tree") +
     n_replicas *  (method == "max_clade_cred" | method == "max_likelihood")
  sim_trees <- TESS::tess.sim.taxa.age(
    n = max_n,
    lambda = lambda_bd,
    mu     = mu_bd,
    nTaxa = ((soc - 1) + length(phylo_brts)), # nolint
    age = age,
    MRCA = TRUE
  )
  if (method == "max_likelihood") {
    liks <- rep(NA, max_n)
    for (n in 1:max_n) {
      liks[n] <-  DDD::bd_loglik(
        pars1 = c(lambda_bd, 0, mu_bd, 0),
        pars2 = c(0, 3, 0, 0, soc - 1),
        brts = ape::branching.times(sim_trees[[n]]),
        missnumspec = 0
      )
    }
    best_n <- which(liks == max(liks))
    bd_tree0 <- sim_trees[[best_n]]
  } else {
    bd_tree0 <- phangorn::maxCladeCred(sim_trees)
  }
  bd_brts0 <- convert_tree2brts(bd_tree0) # nolint pirouette function

  bd_tree <- combine_brts_and_topology(
    brts = bd_brts0,
    tree = phylogeny
  )

  bd_l_matrix <- bd_phylo_2_l_table(bd_tree) # nolint

  list(
    tree = bd_tree,
    l_matrix = bd_l_matrix
  )
}
