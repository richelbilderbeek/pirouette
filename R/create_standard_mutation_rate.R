#' Create the parameters for the mutation rate
#' @inheritParams default_params_doc
#' @return the mutation rate
#' @export
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
create_standard_mutation_rate <- function(
  crown_age
) {
  testit::assert(is.numeric(crown_age))
  testit::assert(crown_age > 0)
  mutation_rate <- 1.0 / crown_age
  mutation_rate
}
