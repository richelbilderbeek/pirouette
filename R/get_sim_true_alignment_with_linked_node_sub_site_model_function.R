#' Get a partially evaluated function
#' to simulate a true alignment with a linked node substitution site model.
#' @inheritParams default_params_doc
#' @return
#' A partially evaluated function of
#' \link{sim_true_alignment_with_linked_node_sub_site_model}
#' @seealso
#' Use \link{get_sim_true_alignment_with_unlinked_node_sub_site_model_function}
#' to get a partially evaluated function
#' to simulate a true alignment with an unlinked node substitution site model.
#' @examples
#' # Create a valid 'check_sim_true_alignment_function'
#' f <- get_sim_true_alignment_with_linked_node_sub_site_model_function()
#' check_sim_true_alignment_function(f)
#'
#' # Simulate a true alignment
#' phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
#' true_alignment <- f(
#'   true_phylogeny = phylogeny,
#'   root_sequence = "acgt"
#' )
#' @export
get_sim_true_alignment_with_linked_node_sub_site_model_function <-
  function(
  subst_matrix = NULL,
  branch_mutation_rate = 1.0,
  node_mutation_rate = 1.0,
  base_frequencies = NULL,
  node_time = 0.001
) {
  pryr::partial(
    sim_true_alignment_with_linked_node_sub_site_model,
    subst_matrix = subst_matrix,
    branch_mutation_rate = branch_mutation_rate,
    node_mutation_rate = node_mutation_rate,
    base_frequencies = base_frequencies,
    node_time = node_time
  )
}
