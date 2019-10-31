#' Get a function to simulate a twin alignment which uses a standard
#' site model
#' @inheritParams default_params_doc
#' @export
get_sim_twin_alignment_with_standard_site_model_function <- function(
  root_sequence,
  mutation_rate,
  site_model = beautier::create_jc69_site_model()
) {
  pirouette::check_root_sequence(root_sequence)
  pirouette::check_mutation_rate(mutation_rate)
  beautier::check_site_model(site_model)
  pryr::partial(
    create_twin_alignment_with_standard_site_model,
    root_sequence = root_sequence,
    mutation_rate = mutation_rate,
    site_model = site_model
  )
}
