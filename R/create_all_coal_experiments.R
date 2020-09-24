#' Create all \link{pirouette} experiments that have a tree prior
#' that follows a coalescent model.
#'
#' These tree priors are both the pure-birth (or Yule) model (as
#' created by \link[beautier]{create_yule_tree_prior}) and the
#' constant-rate birth-death model (as
#' created by \link[beautier]{create_bd_tree_prior}).
#'
#' These experiments are used in the \link{create_pir_params} function
#' @inheritParams default_params_doc
#' @return all \link{pirouette} experiments.
#' @seealso
#' \itemize{
#'   \item Use \link{create_all_experiments} to create experiments with
#'     all combinations of tree model, clock model and tree priors
#'   \item Use \link{create_all_bd_experiments} to create experiments
#'     with all combinations of tree model, clock model and tree priors,
#'     except for only using birth-death tree priors
#'   \item Use \link{create_all_coal_experiments} to create all experiments
#'     with all combinations of tree model, clock model and tree priors,
#'     except for only coalescent tree priors
#' }
#' @author Richèl J.C. Bilderbeek
#' @examples
#'
#' if (rappdirs::app_dir()$os != "win" && beastier::is_on_travis()) {
#'    # it does not work on Windows
#'    experiments <- create_all_coal_experiments()
#'    check_experiments(experiments)
#'
#'    length(experiments) >= 24)
#'
#'    pir_params <- create_pir_params(
#'      alignment_params = create_test_alignment_params(),
#'      experiments = experiments,
#'      evidence_filename = get_temp_evidence_filename()
#'    )
#' }
#'
#' @export
create_all_coal_experiments <- function(
  site_models = beautier::create_site_models(),
  clock_models = beautier::create_clock_models(),
  tree_priors = list(
    beautier::create_cbs_tree_prior(),
    beautier::create_ccp_tree_prior(),
    beautier::create_cep_tree_prior()
  ),
  mcmc = beautier::create_mcmc(store_every = 1000),
  exclude_model = NA
) {
  tree_prior_names <- unlist(
    lapply(tree_priors, function(x) x$name)
  )
  testthat::expect_true(
    all(
      tree_prior_names %in% c(
        "coalescent_bayesian_skyline",
        "coalescent_constant_population",
        "coalescent_exp_population"
      )
    )
  )

  pirouette::create_all_experiments(
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors,
    mcmc = mcmc,
    exclude_model = exclude_model
  )
}
