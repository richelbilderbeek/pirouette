#' Create a twin tree from a phylogeny using a Yule process
#' @inheritParams default_params_doc
#' @return a twin Yule tree of class \code{phylo},
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_yule_tree <- function(
  phylogeny,
  twinning_params
) {
  seed <- twinning_params$rng_seed
  method <- twinning_params$method
  n_replicas <- twinning_params$n_replicas

  age <- beautier::get_crown_age(phylogeny)
  phylo_brts <- sort(
    convert_tree2brts(phylogeny), # nolint pirouette function
    decreasing = TRUE
  )
  n_tips <- ape::Ntip(phylogeny)
  soc <- 1 + n_tips - length(phylo_brts)
  testit::assert(soc == 1 | soc == 2)
  lambda <- log(n_tips) / age
  mu <- 0

  if (rappdirs::app_dir()$os != "win") {
    sink(file.path(rappdirs::user_cache_dir(), "ddd"))
  } else {
    sink(rappdirs::user_cache_dir())
  }
  # TODO: Issue #52: check the quality of inference of lambda and mu provided by bd_ML # nolint
  yule_pars <- DDD::bd_ML(
    brts = sort(phylo_brts, decreasing = TRUE),
    cond = 1, #conditioning on stem or crown age # nolint
    initparsopt = c(lambda),
    idparsopt = c(1),
    idparsfix = c(2, 3, 4),
    parsfix = c(mu, 0, 0),
    missnumspec = 0,
    tdmodel = 0,
    btorph = 1,
    soc = soc
  )
  sink()

  lambda_yule <- as.numeric(unname(yule_pars[1]))
  mu_yule <- mu
  testit::assert(!is.null(lambda_yule))
  testit::assert(is.numeric(lambda_yule))

  # generate bd branching times from the inferred parameters
  set.seed(seed)
  max_n <- 1 * (method == "random_tree") +
     n_replicas *  (method == "max_clade_cred" | method == "max_likelihood")
  sim_trees <- TESS::tess.sim.taxa.age(
    n = max_n,
    lambda = lambda_yule,
    mu     = mu_yule,
    nTaxa = ((soc - 1) + length(phylo_brts)), # nolint
    age = age,
    MRCA = TRUE
  )
  if (method == "max_likelihood") {
    liks <- rep(NA, max_n)
    for (n in 1:max_n) {
      liks[n] <-  DDD::bd_loglik(
        pars1 = c(lambda_yule, 0, mu_yule, 0),
        pars2 = c(0, 3, 0, 0, soc - 1),
        brts = ape::branching.times(sim_trees[[n]]),
        missnumspec = 0
      )
    }
    best_n <- which(liks == max(liks))
    yule_tree0 <- sim_trees[[best_n]]
  } else {
    yule_tree0 <- phangorn::maxCladeCred(sim_trees)
  }
  yule_brts0 <- convert_tree2brts(yule_tree0) # nolint pirouette function

  yule_tree <- combine_brts_and_topology(
    brts = yule_brts0,
    tree = phylogeny
  )

  yule_l_matrix <- bd_phylo_2_l_table(yule_tree) # nolint

  list(
    tree = yule_tree,
    l_matrix = yule_l_matrix
  )
}
