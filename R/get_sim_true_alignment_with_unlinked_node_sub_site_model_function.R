#' Create a function to simulate a true alignment with an unlinked
#' node substitution site model
#' @inheritParams default_params_doc
#' @examples
#' f <- get_sim_true_alignment_with_unlinked_node_sub_site_model_function(
#'   root_sequence = "acgt"
#' )
#' check_sim_true_alignment_function(f)
#' phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
#' true_alignment <- f(true_phylogeny = phylogeny)
#' @export
get_sim_true_alignment_with_unlinked_node_sub_site_model_function <-
  function(
  branch_subst_matrix = NULL,
  node_subst_matrix = 1.0,
  branch_mutation_rate = 1.0,
  node_mutation_rate = 1.0,
  base_frequencies = NULL,
  root_sequence,
  node_time = 0.001
) {
  pryr::partial(
    sim_true_alignment_with_unlinked_node_sub_site_model,
    branch_subst_matrix = branch_subst_matrix,
    node_subst_matrix = node_subst_matrix,
    branch_mutation_rate = branch_mutation_rate,
    node_mutation_rate = node_mutation_rate,
    base_frequencies = base_frequencies,
    root_sequence = root_sequence,
    node_time = node_time
  )
}
