#' Create an "ideal" tree
#' @inheritParams default_params_doc
#' @export
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
create_ideal_tree <- function(
  n_taxa,
  crown_age,
  n_0 = 2,
  ideal_method = "stunning"
) {
  if (!(ideal_method %in% get_ideal_methods())) {
   stop("This 'ideal_method' is not implemented")
  }
  if (ideal_method == "stunning") {
    ideal_tree <- create_stunning_tree(
      t_0 = crown_age,
      n_taxa = n_taxa,
      n_0 = n_0
    )
  }
  testit::assert(beautier::is_phylo(ideal_tree))
  ideal_tree
}
