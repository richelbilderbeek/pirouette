#' Converts a phylogeny to a random DNA alignment
#' @inheritParams default_params_doc
#' @return an alignment of type \code{DNAbin}
#' @seealso Use \link{sim_alignment_file} to save the simulated alignment
#'   directly to a file
#' @examples
#' library(testthat)
#'
#' # Create the ancestor's DNA sequence
#' n_base_pairs <- 4
#' root_sequence <- create_blocked_dna(length = n_base_pairs)
#'
#' # How to simulate the alignment
#' alignment_params <- create_alignment_params(
#'   root_sequence = root_sequence,
#'   mutation_rate = 0.1
#' )
#'
#' # Create a phylogeny to simulate the DNA sequences on
#' n_taxa <- 5
#' phylogeny <- ape::rcoal(n_taxa)
#'
#' # Simulate the alignment
#' alignment <- sim_alignment(
#'    phylogeny = phylogeny,
#'    alignment_params = alignment_params
#'  )
#'
#' expect_equal(class(alignment), "DNAbin")
#' expect_equal(nrow(alignment), n_taxa)
#' expect_equal(ncol(alignment), n_base_pairs)
#'
#' # Use all different site models
#' for (site_model in create_site_models()) {
#'   alignment_params <- create_alignment_params(
#'     root_sequence = root_sequence,
#'     mutation_rate = 0.1,
#'     site_model = site_model
#'   )
#'   expect_silent(
#'     sim_alignment(
#'       phylogeny = phylogeny,
#'       alignment_params = alignment_params
#'     )
#'   )
#' }
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @export
sim_alignment <- function(
  phylogeny,
  alignment_params,
  n_mutations = NA
) {
  beautier::check_phylogeny(phylogeny)
  if (!is.null(geiger::is.extinct(phylogeny))) {
    stop("phylogeny must not contain extant species")
  }
  tryCatch(
    check_alignment_params(alignment_params),  # nolint pirouette function
    error = function(msg) {
      msg <- paste0(
        "'alignment_params' must be a set of alignment parameters. ",
        msg
      )
      stop(msg)
    }
  )

  sim_alignment_raw(
    phylogeny = phylogeny,
    root_sequence = alignment_params$root_sequence,
    rng_seed = alignment_params$rng_seed,
    mutation_rate = alignment_params$mutation_rate,
    site_model = alignment_params$site_model,
    n_mutations = n_mutations
  )
}

#' Converts a phylogeny to a random DNA alignment
#' @inheritParams default_params_doc
#' @return an alignment of type \code{DNAbin}
#' @seealso Use \link{sim_alignment_file} to save the simulated alignment
#'   directly to a file
#' @examples
#' library(testthat)
#'
#' # Create the ancestor's DNA sequence
#' n_base_pairs <- 4
#' root_sequence <- create_blocked_dna(length = n_base_pairs)
#' mutation_rate <- 0.1
#' n_taxa <- 5
#' rng_seed <- 314
#' phylogeny <- ape::rcoal(n_taxa)
#' site_model <- create_jc69_site_model()
#'
#' # Simulate the alignment
#' alignment <- sim_alignment_raw(
#'    phylogeny = phylogeny,
#'    root_sequence = root_sequence,
#'    rng_seed = rng_seed,
#'    mutation_rate = mutation_rate,
#'    site_model = site_model
#'  )
#'
#' expect_equal(class(alignment), "DNAbin")
#' expect_equal(nrow(alignment), n_taxa)
#' expect_equal(ncol(alignment), n_base_pairs)
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @export
sim_alignment_twin <- sim_alignment_raw <- function(
  phylogeny,
  root_sequence,
  rng_seed,
  mutation_rate,
  site_model,
  n_mutations = NA
) {
  beautier::check_phylogeny(phylogeny)
  if (!is.null(geiger::is.extinct(phylogeny))) {
    stop("phylogeny must not contain extant species")
  }
  if (!(is.na(n_mutations) || floor(n_mutations) == ceiling(n_mutations))) {
    stop(
      "n_mutations must be integer or NA (meaning no constraint on the number)"
    )
  }
  set.seed(rng_seed)

  correct_n_mutations <- FALSE
  while (correct_n_mutations == FALSE) {
    alignment_phydat <- phangorn::simSeq(
      phylogeny,
      l = nchar(root_sequence),
      rate = mutation_rate,
      rootseq = strsplit(root_sequence, split = "")[[1]],
      Q = create_rate_matrix(
        site_model = site_model,
        base_frequencies = calc_base_freq(root_sequence)
      )
    )

    testit::assert(class(alignment_phydat) == "phyDat")

    alignment_dnabin <- ape::as.DNAbin(alignment_phydat)

    sim_mutations <- count_n_mutations(
      alignment = alignment_dnabin,
      root_sequence = root_sequence
    )

    correct_n_mutations <- (sim_mutations == n_mutations) || is.na(n_mutations)
  }

  testit::assert(nrow(alignment_dnabin) == length(phylogeny$tip.label))
  testit::assert(ncol(alignment_dnabin) == nchar(root_sequence))

  alignment_dnabin
}
