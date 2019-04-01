#' Create a twin tree from a phylogeny using a Yule process
#' @inheritParams default_params_doc
#' @return a twin Yule tree of class \code{phylo},
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @export
twin_to_yule_tree <- function(
  phylogeny,
  twinning_params
) {
  seed <- twinning_params$rng_seed
  method <- twinning_params$method
  n_replicates <- twinning_params$n_replicates

  age <- beautier::get_crown_age(phylogeny)
  phylo_brts <- sort(
    convert_tree2brts(phylogeny), # nolint pirouette function
    decreasing = TRUE
  )
  n_tips <- ape::Ntip(phylogeny)
  soc <- 1 + n_tips - length(phylo_brts)
  testit::assert(soc == 1 | soc == 2)
  difference <- (log(n_tips) - log(soc)) / age
  mu <- 0
  lambda <- mu + difference

  if (rappdirs::app_dir()$os != "win") {
    sink(tempfile())
  } else {
    sink(rappdirs::user_cache_dir())
  }
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
  yule_brts0 <- create_twin_branching_times(
    phylogeny = phylogeny,
    seed = seed,
    lambda = lambda_yule,
    mu = mu_yule,
    n_replicates = n_replicates,
    method = method
  )

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
