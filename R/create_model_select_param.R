#' Create model selection parameters.
#'
#' Determine the strategy to select between multiple models to
#' be used in inference:
#'
#' \itemize{
#'   \item \link{create_gen_model_select_param}: do not select between
#'     inference models: use the generative model
#'   \item \link{create_best_model_select_param}: select the
#'     inference model with the most evidence (aka marginal likelihood),
#'     and use that model for inference
#' }
#' The simplest combination uses \code{model_selections = "generative"},
#' in which the generative model of the alignmnent is (rightfully)
#' assumed to underly its creation. The tree prior underlying the phylogeny
#' is assumed to be a Birth-Death prior.
#' For a combination such as \code{model_selections = "most_evidence"},
#' @inheritParams default_params_doc
#' @examples
#'   # Pick the generative model
#'   alignment_params <- create_alignment_params(
#'     root_sequence = "acgt", mutation_rate = 0.01
#'   )
#'   model_select_param <- create_gen_model_select_param(alignment_params)
#'   # In such a case, the site model and clock models of the alignment is
#'   # stored in the model selection parameters
#'   testthat::expect_equal(
#'     alignment_params$site_model,
#'     model_select_param$site_models[[1]]
#'   )
#'   testthat::expect_equal(
#'     alignment_params$clock_model,
#'     model_select_param$clock_models[[1]]
#'   )
#'   # By default, a birth-death model is assumed to underly the phylogeny
#'   testthat::expect_equal(
#'     model_select_param$tree_priors[[1]],
#'     beautier::create_bd_tree_prior()
#'   )
#'
#'   # Pick the model with most evidence to be used in inference
#'   model_select_param <- create_best_model_select_param()
#'   # In such a case, multiple site models, clock models
#'   #  and tree priors are tested
#'   testthat::expect_true(length(model_select_param$site_models) > 1)
#'   testthat::expect_true(length(model_select_param$clock_models) > 1)
#'   testthat::expect_true(length(model_select_param$tree_priors) > 1)
#' @author Richel J.C. Bilderbeek
#' @export
create_model_select_param <- function(
  type,
  site_models = beautier::create_site_models(),
  clock_models = beautier::create_clock_models(),
  tree_priors = beautier::create_tree_priors(),
  epsilon = 1e-12,
  marg_lik_filename = tempfile(fileext = ".csv")
) {
  beautier::check_site_models(site_models)
  beautier::check_clock_models(clock_models)
  beautier::check_tree_priors(tree_priors)
  model_select_param <- list(
    type = type,
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors,
    epsilon = epsilon,
    marg_lik_filename = marg_lik_filename
  )
  beautier::check_site_models(model_select_param$site_models)
  beautier::check_clock_models(model_select_param$clock_models)
  beautier::check_tree_priors(model_select_param$tree_priors)
  model_select_param
}

#' Select the generative model to be used in inference
#'
#' Create model selection parameters
#' in which the generative model of the alignmnent is (rightfully)
#' assumed to underly its creation.
#' The tree prior underlying the phylogeny is assumed to be a Birth-Death prior.
#' @inheritParams default_params_doc
#' @seealso \link{create_model_select_param} contains an overview
#'   of all model selection possible
#' @examples
#'   # Pick the generative model
#'   alignment_params <- create_alignment_params(
#'     root_sequence = "acgt", mutation_rate = 0.01
#'   )
#'   model_select_param <- create_gen_model_select_param(alignment_params)
#'   # In such a case, the site model and clock models of the alignment is
#'   # stored in the model selection parameters
#'   testthat::expect_equal(
#'     alignment_params$site_model,
#'     model_select_param$site_models[[1]]
#'   )
#'   testthat::expect_equal(
#'     alignment_params$clock_model,
#'     model_select_param$clock_models[[1]]
#'   )
#'   # By default, a birth-death model is assumed to underly the phylogeny
#'   testthat::expect_equal(
#'     model_select_param$tree_priors[[1]],
#'     beautier::create_bd_tree_prior()
#'   )
#' @author Richel J.C. Bilderbeek
#' @export
create_gen_model_select_param <- function(
  alignment_params = create_alignment_params(),
  tree_prior = beautier::create_bd_tree_prior()
) {
  check_alignment_params(alignment_params) # nolint pirouette function
  beautier::check_tree_prior(tree_prior)
  model_select_param <- create_model_select_param(
    type = "generative",
    site_models = list(alignment_params$site_model),
    clock_models = list(alignment_params$clock_model),
    tree_priors = list(tree_prior),
    epsilon = NA
  )
  testit::assert(length(model_select_param$site_models) == 1)
  testit::assert(length(model_select_param$clock_models) == 1)
  testit::assert(length(model_select_param$tree_priors) == 1)
  model_select_param
}

#' Create model selection parameters
#' for the model with the model evidence (also: marginal likelihood)
#' from a set of any combination of site models, clock models and
#' tree priors.
#' @inheritParams default_params_doc
#' @seealso \link{create_model_select_param} contains an overview
#'   of all model selection possible
#' @examples
#'   # Pick the model with most evidence to be used in inference
#'   model_select_param <- create_best_model_select_param()
#'   # In such a case, multiple site models, clock models
#'   #  and tree priors are tested
#'   testthat::expect_true(length(model_select_param$site_models) > 1)
#'   testthat::expect_true(length(model_select_param$clock_models) > 1)
#'   testthat::expect_true(length(model_select_param$tree_priors) > 1)
#' @author Richel J.C. Bilderbeek
#' @export
create_best_model_select_param <- function( # nolint indeed a long function name
  site_models = beautier::create_site_models(),
  clock_models = beautier::create_clock_models(),
  tree_priors = beautier::create_tree_priors(),
  epsilon = 1e-12,
  marg_lik_filename = tempfile(fileext = ".csv")
) {
  testit::assert(beautier::are_site_models(site_models))
  testit::assert(beautier::are_clock_models(clock_models))
  testit::assert(beautier::are_tree_priors(tree_priors))
  testit::assert(!beautier::is_site_model(site_models))
  testit::assert(!beautier::is_clock_model(clock_models))
  testit::assert(!beautier::is_tree_prior(tree_priors))

  create_model_select_param(
    type = "most_evidence",
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors,
    epsilon = epsilon,
    marg_lik_filename = marg_lik_filename
  )
}
