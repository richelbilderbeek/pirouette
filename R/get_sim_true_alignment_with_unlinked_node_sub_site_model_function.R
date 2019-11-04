#' Create a function to simulate a true alignment with an unlinked
#' node substitution site model
#' @inheritParams default_params_doc
#' @export
get_sim_true_alignment_with_unlinked_node_sub_site_model_function <-
  function(
  root_sequence
) {
  pryr::partial(
    sim_true_alignment_with_unlinked_node_sub_site_model,
    root_sequence = root_sequence
  )
}
