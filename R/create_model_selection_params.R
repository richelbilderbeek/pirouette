#' Create model selection parameters.
#' The simplest combination uses \code{model_selections = "generative"},
#' in which the generative model of the alignmnent is (rightfully)
#' assumed to underly its creation. The tree prior underlying the phylogeny
#' is assumed to be a Birth-Death prior.
#' For a combination such as \code{model_selections = "most_evidence"},
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @export
create_model_select_params <- function(
  model_selections = "generative",
  site_models = beautier::create_site_models(),
  clock_models = beautier::create_clock_models(),
  tree_priors = beautier::create_tree_priors()
) {
  beautier::check_site_models(site_models)
  beautier::check_clock_models(clock_models)
  beautier::check_tree_priors(tree_priors)
  model_select_params <- list(
    model_selections = model_selections,
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors
  )
  beautier::check_site_models(model_select_params$site_models)
  beautier::check_clock_models(model_select_params$clock_models)
  beautier::check_tree_priors(model_select_params$tree_priors)
  model_select_params
}

#' Create model selection parameters
#' in which the generative model of the alignmnent is (rightfully)
#' assumed to underly its creation.
#' The tree prior underlying the phylogeny is assumed to be a Birth-Death prior.
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @export
create_gen_model_select_params <- function(
  alignment_params,
  tree_prior = beautier::create_bd_tree_prior()
) {
  check_alignment_params(alignment_params)
  beautier::check_tree_prior(tree_prior)
  create_model_select_params(
    model_selections = "generative",
    site_models = alignment_params$site_model,
    clock_models = alignment_params$clock_model,
    tree_priors = tree_prior
  )
}

#' Create model selection parameters
#' for the model with the model evidence (also: marginal likelihood)
#' from a set of any combination of site models, clock models and
#' tree priors.
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @export
create_most_evidence_model_select_params <- function(
  site_models = beautier::create_site_models(),
  clock_models = beautier::create_clock_models(),
  tree_priors = beautier::create_tree_priors()
) {
  create_model_select_params(
    model_selections = "most_evidence",
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors
  )
}
