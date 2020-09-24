#' Measure the error BEAST2 makes from a known phylogeny.
#'
#' From a phylogeny of (un)known speciation model,
#' an alignment is created using a known site model and clock model,
#' as given by \code{alignment_params}.
#'
#' @inheritParams default_params_doc
#' @return a data frame with errors, with as many rows as model selection
#' parameter sets. The output can be checked using \link{check_pir_out}.
#' @seealso
#' \itemize{
#'   \item Use \link{pir_plot} to display the output of \link{pir_run} as a
#'     figure.
#'   \item Use \link{create_test_pir_run_output} to create a test output
#'     of \link{pir_run}.
#'   \item Use \link{pir_runs} to do multiple \link{pirouette} runs.
#' }
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @examples
#'
#' phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
#' pir_params <- create_test_pir_params()
#'
#' errors <- NA
#' if (
#'   rappdirs::app_dir()$os != "win" &&
#'   is_on_ci() &&
#'   is_beast2_installed()
#' ) {
#'   errors <- pir_run(
#'     phylogeny = phylogeny,
#'     pir_params = pir_params
#'   )
#' } else {
#'   errors <- create_test_pir_run_output()
#' }
#'
#' # Return value
#' "tree" %in% names(errors))
#' is.factor(errors$tree))
#' "true" %in% errors$tree)
#'
#' "inference_model" %in% names(errors))
#' is.factor(errors$inference_model))
#' "generative" %in% errors$inference_model)
#'
#' "inference_model_weight" %in% names(errors))
#' !is.factor(errors$inference_model_weight))
#'
#' "site_model" %in% names(errors))
#' is.factor(errors$site_model))
#' "JC69" %in% errors$site_model)
#'
#' "clock_model" %in% names(errors))
#' is.factor(errors$clock_model))
#'
#' "tree_prior" %in% names(errors))
#' is.factor(errors$tree_prior))
#' "birth_death" %in% errors$tree_prior ||
#'   "yule" %in% errors$tree_prior
#' )
#'
#' "error_1" %in% names(errors))
#' !is.factor(errors$error_1))
#'
#' # Errors more than zero
#' col_first_error <- which(colnames(errors) == "error_1")
#' col_last_error <- ncol(errors)
#' all(errors[, col_first_error:col_last_error] > 0.0))
#' n_errors <- col_last_error - col_first_error + 1
#' n_errors < 11) # due to burn-in
#' @export
pir_run <- function(
  phylogeny,
  pir_params = create_pir_params(
    alignment_params = create_alignment_params(),
    twinning_params = create_twinning_params()
  )
) {
  # Check the inputs
  beautier::check_phylogeny(phylogeny)
  pirouette::check_pir_params(pir_params)
  testit::assert(beastier::is_beast2_installed())
  if (pirouette::has_candidate_experiments(pir_params)) {
    testit::assert(mauricer::is_beast2_ns_pkg_installed())
  }

  # Higher-level checks
  for (experiment in pir_params$experiments) {
    if (beautier::is_cbs_tree_prior(experiment$inference_model$tree_prior) &&
        ape::Ntip(phylogeny) < 6) {
      stop("Too few taxa to use a Coalescent Bayesian Skyline tree prior")
    }
  }

  # Initialize the pir_params, as some BEAUti defaults are handy,
  # but really need to be filled with all information at hand
  pir_params <- pirouette::init_pir_params(pir_params)

  # Run for the true tree
  pir_out <- pir_run_true_tree(
    true_phylogeny = phylogeny,
    pir_params = pir_params
  )

  # Run for the twin tree
  if (pirouette::has_twinning(pir_params)) {

    # Create twin tree
    twin_tree <- pirouette::create_twin_tree(
      phylogeny,
      twinning_params = pir_params$twinning_params
    )

    # Save twin tree
    twin_tree_filename <- pir_params$twinning_params$twin_tree_filename
    # Create a sub-sub-sub folder, don't warn when it already exists
    dir.create(
      dirname(twin_tree_filename),
      showWarnings = FALSE, recursive = TRUE
    )
    ape::write.tree(phy = twin_tree, file = twin_tree_filename)
    beautier::check_file_exists(twin_tree_filename, "twin_tree_filename")

    pir_out_twin <- pirouette::pir_run_twin_tree(
      twin_phylogeny = twin_tree,
      pir_params = pir_params
    )
    pir_out <- rbind(pir_out, pir_out_twin)
  }
  pir_out
}
