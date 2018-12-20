#' Create the parameters for the alignment simulation
#' @inheritParams default_params_doc
#' @return a list of alignment parameters
#' @export
#' @author Richel J.C. Bilderbeek
create_alignment_params <- function(
  root_sequence,
  mutation_rate
) {
  list(
    root_sequence = root_sequence,
    mutation_rate = mutation_rate
  )
}
