#' Create the parameters for the mutation rate
#' @inheritParams default_params_doc
#' @return the mutation rate
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#'
#' # Phylogeny with a crown age of 3.0
#' phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);")
#' # Expected mutation rate is one divided by the crown age
#' create_standard_mutation_rate(phylogeny) # 0.333
#'
#' # Phylogeny with a crown age of 4.0
#' phylogeny <- ape::read.tree(text = "((A:2, B:2):2, C:4);")
#' # Expected mutation rate is one divided by the crown age
#' create_standard_mutation_rate(phylogeny) # 0.25
#'
#' @export
create_standard_mutation_rate <- function(
  phylogeny
) {
  beautier::check_phylogeny(phylogeny)
  crown_age <- beautier::get_crown_age(phylogeny)
  testit::assert(crown_age > .Machine$double.xmin)
  mutation_rate <- 1.0 / crown_age
  mutation_rate
}
