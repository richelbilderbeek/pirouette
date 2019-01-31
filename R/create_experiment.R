#' Create a \link{pirouette} experiment.
#' @inheritParams default_params_doc
#' @return a \link{pirouette} experiment.
#' @examples
#'  experiment <- create_experiment()
#'  testit::assert("rng_seed" %in% names(experiment))
#'  testit::assert("twin_tree_filename" %in% names(experiment))
#' @export
#' @author Richel J.C. Bilderbeek
create_experiment <- function(
  rng_seed = 0,
  twin_model = "bd",
  twin_tree_filename = tempfile(fileext = ".newick"),
  twin_alignment_filename = tempfile(fileext = ".fasta")
) {
  experiment <- list(
    rng_seed = rng_seed,
    twin_model = twin_model,
    twin_tree_filename = twin_tree_filename,
    twin_alignment_filename = twin_alignment_filename
  )
  check_experiment(experiment = experiment) # nolint pirouette function
  experiment
}
