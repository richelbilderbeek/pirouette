#' Creates a twin alignment.
#'
#' A twin alignment is an alignment that has as much acquired
#' the same number of mutations (compared to the root sequence),
#' as the true alingment has (compared to the root sequence).
#'
#' When an alignment gets very big, say one million base pairs,
#' it will take long to get a twin alignment with exactly the same
#' number of mutations.
#' @inheritParams default_params_doc
#' @return an alignment of class DNAbin that has as much
#'   mutations accumulated from crown to the tips as the
#'   original, 'true' alignment
#' @examples
#' true_phylogeny <- ape::read.tree(text = "((A:1, B:1):2, C:3);")
#' twin_phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);")
#' root_sequence <- create_blocked_dna(1000)
#' alignment_params <- create_test_alignment_params(
#'   root_sequence = root_sequence,
#'   rng_seed = 314,
#'   mutation_rate = 0.001
#' )
#' true_alignment <- create_true_alignment(
#'   true_phylogeny = true_phylogeny,
#'   alignment_params = alignment_params
#' )
#' n_mutations_true <- count_n_mutations(
#'   alignment = true_alignment, root_sequence = root_sequence
#' )
#'
#' twin_alignment <- create_twin_alignment(
#'   twin_phylogeny = twin_phylogeny,
#'   true_alignment = true_alignment,
#'   alignment_params = alignment_params,
#'   twinning_params = create_twinning_params()
#' )
#' n_mutations_twin <- count_n_mutations(
#'   alignment = twin_alignment, root_sequence = root_sequence
#' )
#'
#' library(testthat)
#' expect_equal(n_mutations_true, n_mutations_twin)
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_twin_alignment <- function(
  twin_phylogeny,
  true_alignment,
  alignment_params,
  twinning_params,
  verbose = FALSE,
  newskool = FALSE
) {
  if (newskool == TRUE) {
    print("NEWSKOOL")
  }
  # Check inputs
  pirouette::check_twin_phylogeny(twin_phylogeny) # nolint pirouette function
  if (class(true_alignment) != "DNAbin") {
    stop("'true_alignmnent' must be a of class 'DNAbin'")
  }
  pirouette::check_alignment_params(alignment_params)
  pirouette::check_twinning_params(twinning_params)

  # Count the goal number of mutations
  n_mutations_true <- count_n_mutations(
    alignment = true_alignment,
    root_sequence = alignment_params$root_sequence
  )

  # Stub: if the sum of branch lengths between the true and twin tree
  # differ, one may want to increase/decrease the mutation rate to
  # reach the same number of mutations more easy.
  #
  # If the tests still pass easily, no worries, and this is great.
  # If the tests take too long, then yes, this correction must be made
  testit::assert(
    beautier::is_one_double(alignment_params$mutation_rate) ||
    is.function(alignment_params$mutation_rate)
  )

  # Will increase the RNG seed up until
  # a twin alignment is found
  rng_seed <- twinning_params$rng_seed_twin_alignment

  # Prepare for the search for the twin alignment
  n_mutations_twin <- -1 # nonsense value
  twin_alignment <- NA

  while (n_mutations_true != n_mutations_twin) {

    twin_alignment_params <- alignment_params
    twin_alignment_params$rng_seed <- rng_seed

    twin_alignment <- create_alignment(
      phylogeny = twin_phylogeny,
      alignment_params = twin_alignment_params
    )
    n_mutations_twin <- count_n_mutations(
      alignment = twin_alignment,
      root_sequence = alignment_params$root_sequence
    )

    rng_seed <- rng_seed + 1

    attempt <- rng_seed - twinning_params$rng_seed_twin_alignment
    if (verbose == TRUE) {
      print(
        paste0(
          "Number of mutations needed: ",
          n_mutations_true,
          ", got: ",
          n_mutations_twin,
          " (attempt: ",
          attempt,
          ")"
        )
      )
    }
  }
  twin_alignment
}
