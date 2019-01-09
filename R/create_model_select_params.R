#' Create model selection parameters.
#'
#' Determine the strategy to select between multiple models to
#' be used in inference:
#'
#' \itemize{
#'   \item \link{create_gen_model_select_params}: do not select between
#'     inference models: use the generative model
#'   \item \link{create_most_evidence_model_select_params}: select the
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
#'   model_select_params <- create_gen_model_select_params(alignment_params)
#'   # In such a case, the site model and clock models of the alignment is
#'   # stored in the model selection parameters
#'   testthat::expect_equal(
#'     alignment_params$site_model,
#'     model_select_params$site_model
#'   )
#'   testthat::expect_equal(
#'     alignment_params$clock_model,
#'     model_select_params$clock_model
#'   )
#'   # By default, a birth-death model is assumed to underly the phylogeny
#'   testthat::expect_equal(
#'     model_select_params$tree_prior,
#'     beautier::create_bd_tree_prior()
#'   )
#'
#'   # Pick the model with most evidence to be used in inference
#'   model_select_params <- create_most_evidence_model_select_params()
#'   # In such a case, multiple site models, clock models
#'   #  and tree priors are tested
#'   testthat::expect_true(length(model_select_params$site_models) > 1)
#'   testthat::expect_true(length(model_select_params$clock_models) > 1)
#'   testthat::expect_true(length(model_select_params$tree_priors) > 1)
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
#' @examples
#'   # Pick the generative model
#'   alignment_params <- create_alignment_params(
#'     root_sequence = "acgt", mutation_rate = 0.01
#'   )
#'   model_select_params <- create_gen_model_select_params(alignment_params)
#'   # In such a case, the site model and clock models of the alignment is
#'   # stored in the model selection parameters
#'   testthat::expect_equal(
#'     alignment_params$site_model,
#'     model_select_params$site_model
#'   )
#'   testthat::expect_equal(
#'     alignment_params$clock_model,
#'     model_select_params$clock_model
#'   )
#'   # By default, a birth-death model is assumed to underly the phylogeny
#'   testthat::expect_equal(
#'     model_select_params$tree_prior,
#'     beautier::create_bd_tree_prior()
#'   )
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
#' @examples
#'   # Pick the model with most evidence to be used in inference
#'   model_select_params <- create_most_evidence_model_select_params()
#'   # In such a case, multiple site models, clock models
#'   #  and tree priors are tested
#'   testthat::expect_true(length(model_select_params$site_models) > 1)
#'   testthat::expect_true(length(model_select_params$clock_models) > 1)
#'   testthat::expect_true(length(model_select_params$tree_priors) > 1)
#' @author Richel J.C. Bilderbeek
#' @export
create_most_evidence_model_select_params <- function( # nolint indeed a long function name
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
