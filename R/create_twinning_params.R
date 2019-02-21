#' Create the parameters for the twinning simulation.
#' The site model and clock models will be used and
#' their combination will be called the generative model
#' of the twinning
#' @inheritParams default_params_doc
#' @param rng_seed the random number generator seed as used in the
#'   simulation of a twin tree
#' @return a twinning parameter set
#' @examples
#'  twinning_params <- create_twinning_params()
#'  testit::assert("rng_seed" %in% names(twinning_params))
#'  testit::assert("twin_tree_filename" %in% names(twinning_params))
#' @export
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
create_twinning_params <- function(
  rng_seed = 0,
  twin_model = "bd",
  method = "stunning",
  n_replicas = 1e4,
  twin_tree_filename = tempfile(fileext = ".newick"),
  twin_alignment_filename = tempfile(fileext = ".fasta"),
  twin_evidence_filename =  tempfile(fileext = ".csv")
) {
  twinning_params <- list(
    rng_seed = rng_seed,
    twin_model = twin_model,
    method = method,
    n_replicas = n_replicas,
    twin_tree_filename = twin_tree_filename,
    twin_alignment_filename = twin_alignment_filename,
    twin_evidence_filename = twin_evidence_filename
  )
  check_twinning_params(twinning_params) # nolint pirouette function
  twinning_params
}
