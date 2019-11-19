#' Adapter function to simulate a twin alignment using a standard site model
#' @inheritParams default_params_doc
#' @examples
#' library(testthat)
#'
#' # This adapter function must be a sim_true_alignment function
#' expect_silent(
#'   check_sim_true_alignment_fun(
#'     sim_true_alignment_with_std_site_model
#'   )
#' )
#'
#' # Simulate the true DNA alignment
#' alignment <- sim_true_alignment_with_std_site_model(
#'   true_phylogeny = ape::read.tree(text = "((A:1, B:1):2, C:3);"),
#'   root_sequence = "aaaa",
#'   mutation_rate = 0.1
#' )
#' expect_silent(check_alignment(alignment))
#' @export
sim_true_alignment_with_std_site_model <- function(
  true_phylogeny,
  root_sequence,
  mutation_rate = 1.0,
  site_model = beautier::create_jc69_site_model()
) {
  alignment <- pirouette::sim_alignment_with_std_site_model(
    phylogeny = true_phylogeny,
    root_sequence = root_sequence,
    mutation_rate = mutation_rate,
    site_model = site_model
  )
  pirouette::check_alignment(alignment)
  alignment
}
