#' Create a function to simulate a true alignment with a linked
#' node substitution site model
#' @export
get_sim_true_alignment_with_linked_node_sub_site_model_function <-
  function(
  root_sequence = "acgt",
  mutation_rate = 0.0
) {
  pryr::partial(
    sim_true_alignment_with_standard_site_model,
    root_sequence = root_sequence,
    mutation_rate = mutation_rate,
    site_model = site_model
  )

}
