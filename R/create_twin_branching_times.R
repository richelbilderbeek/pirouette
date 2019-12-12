#' Generate twin branching times given estimated lambda and mu and the original
#'   tree
#' @inheritParams default_params_doc
#' @return twin branching times
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#' phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
#'
#' branching_times <- create_twin_branching_times(
#'   lambda = 0.1,
#'   mu = 1.0,
#'   phylogeny = phylogeny,
#'   n_replicates = 1,
#'   method = "random_tree"
#' )
#'
#' library(testthat)
#' expect_equal(
#'   length(ape::branching.times(phylogeny)),
#'   length(branching_times)
#' )
#' @export
create_twin_branching_times <- function(
  lambda,
  mu,
  phylogeny,
  n_replicates,
  method
) {
  if (!beautier::is_one_double(lambda)) {
    stop("'lambda' must be one numerical value")
  }
  if (!beautier::is_one_double(mu)) {
    stop("'mu' must be one numerical value")
  }
  beautier::check_phylogeny(phylogeny)
  if (!beautier::is_one_int(n_replicates)) {
    stop("'n_replicates' must be one numerical value")
  }
  methods <- c("random_tree", "max_clade_cred", "max_likelihood")
  if (!method %in% methods) {
    stop(
      "'method' not in the supported methods. \n",
      "Supported methods: ", methods, ". \n",
      "Actual value: ", method
    )
  }

  # Retrieve info from the phylogeny
  age  <- beautier::get_crown_age(phylogeny)
  phylo_brts <- sort(
    pirouette::convert_tree2brts(phylogeny),
    decreasing = TRUE
  )
  n_tips <- ape::Ntip(phylogeny)
  soc <- 1 + n_tips - length(phylo_brts)

  # Simulate branching times
  max_n <- 1 * (method == "random_tree") +
     n_replicates * (method == "max_clade_cred" | method == "max_likelihood")
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
  } else {
    testit::assert(method == "max_clade_cred" || method == "random_tree")
    tree0 <- phangorn::maxCladeCred(sim_trees)
  }
  brts0 <- pirouette::convert_tree2brts(tree0)
  brts0
}
