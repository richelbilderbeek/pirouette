#' Checks if the argument is a valid alignment parameters structure,
#' as created by \link{create_alignment_params}.
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if nit
#' @author Richel J.C. Bilderbeek
check_alignment_params <- function(
  alignment_params
) {
  argument_names <- c(
    "root_sequence",
    "mutation_rate",
    "rng_seed",
    "site_model",
    "clock_model",
    "fasta_filename"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(alignment_params)) {
      stop(
        "'", arg_name, "' must be an element of an 'alignment_params'. ",
        "Tip: use 'create_alignment_params'"
      )
    }
  }
  if (!is_dna_seq(alignment_params$root_sequence)) {
    stop("'root_sequence' must be a lowercase DNA character string")
  }
  if (is.function(alignment_params$mutation_rate)) {
    phylogeny_1 <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
    test_1 <- alignment_params$mutation_rate(phylogeny_1)
    phylogeny_2 <- load_tree(tree_model = "mbd", seed = 1) # nolint pirouette function
    test_2 <- alignment_params$mutation_rate(phylogeny_2)
    if (!is.numeric(test_1) | !is.numeric(test_2)) {
      stop("'mutation_rate' function must return a number")
    } else {
      if (test_1 < 0 | test_2 < 0) {
        stop("'mutation_rate' function must return non-zero and positive value")
      }
    }
  } else {
    if (alignment_params$mutation_rate < 0) {
      stop("'mutation_rate' must be a non-zero and positive value")
    }
  }
  if (!is.numeric(alignment_params$rng_seed)) {
    stop("'rng_seed' must be a number")
  }
}
