#' Create a valid testing \link{pirouette} candidate experiment.
#' @inheritParams default_params_doc
#' @return a \link{pirouette} experiment.
#' @author Richèl J.C. Bilderbeek
#' @examples
#' if (beautier::is_on_ci()) {
#'
#'   # Create a candidate experiment
#'   if (rappdirs::app_dir()$os != "win") {
#'     # it does not work on Windows
#'     experiment <- create_cand_experiment()
#'     check_experiment(experiment)
#'   }
#'
#'   # Create a generative experiment
#'   experiment <- create_gen_experiment()
#'   check_experiment(experiment)
#'
#'   # Use the experiment to create the full pirouette parameter set
#'   pir_params <- create_pir_params(
#'     alignment_params = create_alignment_params(),
#'     experiments = list(experiment)
#'   )
#'
#'   if (rappdirs::app_dir()$os != "win" &&
#'     beautier::is_on_ci() && beastier::is_beast2_installed()
#'   ) {
#'     pir_out <- pir_run(
#'       phylogeny = ape::read.tree(text = "((A:2, B:2):1, C:3);"),
#'       pir_params = pir_params
#'     )
#'     pir_plot(pir_out)
#'   }
#'
#' }
#' @export
create_cand_experiment <- function(
  inference_conditions = create_inference_conditions(
    model_type = "candidate",
    run_if = "best_candidate",
    do_measure_evidence = TRUE
  ),
  inference_model = beautier::create_inference_model(
    mcmc = beautier::create_mcmc(store_every = 1000)
  ),
  beast2_options = beastier::create_beast2_options(),
  est_evidence_mcmc = beautier::create_ns_mcmc(
    store_every = 1000,
    epsilon = 1e-12
  )
) {
  pirouette::create_experiment(
    inference_conditions = inference_conditions,
    inference_model = inference_model,
    beast2_options = beast2_options,
    est_evidence_mcmc = est_evidence_mcmc
  )
}
