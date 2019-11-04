#' Get a function to simulate the true alignment with,
#' that uses a standard site model.
#' @inheritParams default_params_doc
#' @examples
#' library(testthat)
#'
#' f <- get_sim_true_alignment_with_standard_site_model_function(
#'   root_sequence = "acgt",
#'   mutation_rate = 0.1
#' )
#' check_sim_true_alignment_function(f)
#'
#' alignment_params <- create_test_alignment_params(
#'   sim_true_alignment_function = f
#' )
#' true_alignment <- pirouette::sim_true_alignment(
#'   true_phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
#'   alignment_params = alignment_params
#' )
#' @export
get_sim_true_alignment_with_standard_site_model_function <- function(
  root_sequence,
  mutation_rate,
  site_model = beautier::create_jc69_site_model()
) {
  pryr::partial(
    sim_true_alignment_with_standard_site_model,
    root_sequence = root_sequence,
    mutation_rate = mutation_rate,
    site_model = site_model
  )
}
