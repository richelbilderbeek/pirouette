#' Create an BD twin tree from a phylogeny
#' and save it as a file
#' @inheritParams default_params_doc
#' @return a twin BD tree of class \link[ape]{phylo},
#'   obtained from the corresponding phylogeny.
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @examples
#' library(testthat)
#'
#' phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);")
#' twinning_params <- create_twinning_params()
#' bd_tree <- twin_to_bd_tree(
#'   phylogeny = phylogeny,
#'   twinning_params = twinning_params
#' )
#'
#' expect_equal(class(bd_tree), "phylo")
#'
#' # Branching times will differ, except the crown
#' expect_false(
#'   all(
#'     ape::branching.times(phylogeny) ==
#'     ape::branching.times(bd_tree)
#'   )
#' )
#'
#' # Crown age stays the same
#' expect_equal(
#'   max(ape::branching.times(bd_tree)),
#'   max(ape::branching.times(phylogeny))
#' )
#' @export
twin_to_bd_tree <- function(
  phylogeny,
  seed = 0,
  method = "random_tree",
  n_replicates = 1e4
) {
  methods <- c("random_tree", "max_clade_cred", "max_likelihood")
  if (!method %in% methods) {
    stop(
      "'method' not in the supported methods. \n",
      "Supported methods: ", methods, ". \n",
      "Actual value: ", method
    )
  }

  age  <- beautier::get_crown_age(phylogeny)
  phylo_brts <- sort(
    pirouette::convert_tree2brts(phylogeny),
    decreasing = TRUE
  )
  n_tips <- ape::Ntip(phylogeny)
  soc <- 1 + n_tips - length(phylo_brts)
  testit::assert(soc == 1 | soc == 2)
  difference <- (log(n_tips) - log(soc)) / age
  mu <- 0.1
  lambda <- mu + difference

  if (rappdirs::app_dir()$os != "win") {
    sink(tempfile())
    bd_pars <- DDD::bd_ML(
      brts = sort(phylo_brts, decreasing = TRUE),
      cond = 1, # conditioning on stem or crown age # nolint
      initparsopt = c(lambda, mu),
      idparsopt = 1:2,
      missnumspec = 0,
      tdmodel = 0,
      btorph = 1,
      soc = soc
    )
    sink()
  } else {
    x <- utils::capture.output({
        bd_pars <- DDD::bd_ML(
          brts = sort(phylo_brts, decreasing = TRUE),
          cond = 1,
          initparsopt = c(lambda, mu),
          idparsopt = 1:2,
          missnumspec = 0,
          tdmodel = 0,
          btorph = 1,
          soc = soc
        )
    }
    )
    rm(x)
  }

  lambda_bd <- as.numeric(unname(bd_pars[1]))
  mu_bd <- as.numeric(unname(bd_pars[2]))
  testit::assert(beautier::is_one_double(lambda_bd))
  testit::assert(beautier::is_one_double(mu_bd))

  # generate bd branching times from the inferred parameters
  bd_brts0 <- create_twin_branching_times(
    phylogeny = phylogeny,
    seed = seed,
    lambda = lambda_bd,
    mu = mu_bd,
    n_replicates = n_replicates,
    method = method
  )

  combine_brts_and_topology(
    brts = bd_brts0,
    tree = phylogeny
  )
}
