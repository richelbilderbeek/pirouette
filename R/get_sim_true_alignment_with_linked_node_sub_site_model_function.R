#' Create a function to simulate a true alignment with a linked
#' node substitution site model
#' @inheritParams default_params_doc
#' @examples
#' # Create a valid 'check_sim_true_alignment_function'
#' f <- get_sim_true_alignment_with_linked_node_sub_site_model_function(
#'   root_sequence = "acgt"
#' )
#' check_sim_true_alignment_function(f)
#'
#' # Simulate a true alignment
#' phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
#' true_alignment <- f(true_phylogeny = phylogeny)
#' @export
get_sim_true_alignment_with_linked_node_sub_site_model_function <-
  function(
  root_sequence
) {
  pryr::partial(
    sim_true_alignment_with_linked_node_sub_site_model,
    root_sequence = root_sequence
  )
}
