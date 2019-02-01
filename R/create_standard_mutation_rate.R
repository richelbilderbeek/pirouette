#' Create the parameters for the mutation rate
#' @inheritParams default_params_doc
#' @return the mutation rate
#' @export
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
create_standard_mutation_rate <- function(
  phylogeny
) {
  if (!beautier::is_phylo(phylogeny)) {
    stop("'phylogeny' must be of class 'phylo'")
  }
  crown_age <- beautier::get_crown_age(phylogeny)
  testit::assert(crown_age > .Machine$double.xmin)
  mutation_rate <- 1.0 / crown_age
  mutation_rate
}
