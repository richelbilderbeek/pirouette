#' Adapter function to simulate a twin alignment using a standard site model
#' @inheritParams default_params_doc
#' @return an alignment
#' @examples
#' # This adapter function must be a sim_true_alignment function
#' check_sim_tral_fun(
#'   sim_tral_with_std_nsm
#' )
#'
#' # Simulate the true DNA alignment
#' alignment <- sim_tral_with_std_nsm(
#'   true_phylogeny = ape::read.tree(text = "((A:1, B:1):2, C:3);"),
#'   root_sequence = "aaaa",
#'   mutation_rate = 0.1
#' )
#' check_alignment(alignment)
#' @author Richèl J.C. Bilderbeek
#' @export
sim_tral_with_std_nsm <- function(
  true_phylogeny,
  root_sequence,
  mutation_rate = 1.0,
  site_model = beautier::create_jc69_site_model()
) {
  alignment <- pirouette::sim_alignment_with_std_nsm(
    phylogeny = true_phylogeny,
    root_sequence = root_sequence,
    mutation_rate = mutation_rate,
    site_model = site_model
  )
  pirouette::check_alignment(alignment)
  alignment
}
