#' Check if the mutation rate is valid
#'
#' Will \link{stop} if not
#' @inheritParams default_params_doc
#' @return nothing
#' @author Richèl J.C. Bilderbeek
#' @export
check_mutation_rate <- function(mutation_rate) {
  if (is.function(mutation_rate)) {
    phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
    mutation_rate <- mutation_rate(phylogeny)
    if (!beautier::is_one_double(mutation_rate)) {
      stop("'mutation_rate' function must return a number")
    }
    if (mutation_rate <= 0.0) {
      stop("'mutation_rate' function must return non-zero and positive value")
    }
  } else {
    if (length(mutation_rate) != 1 ||
        mutation_rate <= 0.0 ||
        is.infinite(mutation_rate)
    ) {
      stop(
        "'mutation_rate' must be one non-zero and finite positive value. \n",
        "Actual value: '", mutation_rate, "'"
      )
    }
  }
  invisible(mutation_rate)
}
