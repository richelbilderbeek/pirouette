#' Get a function to simulate the true alignment with,
#' that uses a standard site model.
#' @inheritParams default_params_doc
#' @return a function
#' @examples
#'
#' f <- get_sim_tral_with_std_nsm_fun(
#'   mutation_rate = 0.1
#' )
#' check_sim_tral_fun(f)
#'
#' alignment_params <- create_test_alignment_params(
#'   sim_tral_fun = f,
#'   root_sequence = "acgt",
#' )
#' true_alignment <- sim_true_alignment(
#'   true_phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
#'   alignment_params = alignment_params
#' )
#' @export
get_sim_tral_with_std_nsm_fun <- function(
  mutation_rate = 1.0,
  site_model = beautier::create_jc69_site_model()
) {
  pirouette::check_mutation_rate(mutation_rate)
  pryr::partial(
    sim_tral_with_std_nsm,
    mutation_rate = mutation_rate,
    site_model = site_model
  )
}
