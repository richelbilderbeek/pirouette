#' Check if the function to create a twin alignment with (from a
#' true phylogeny and a true alignment) is valid.
#'
#' Will \link{stop} if not
#' @inheritParams default_params_doc
#' @seealso
#' Use \link{check_sim_tral_fun} to check a function to
#' generate a true alignment.
#' Use \link{check_sim_twin_tree_fun} to check a function to
#' generate a twin phylogeny.
#' @export
check_sim_twal_fun <- function(sim_twal_fun) {
  if (!is.function(sim_twal_fun)) {
    stop("'sim_twal_fun' must be a function")
  }

  # sim_twal_fun must return a DNAbin
  # Use a simple testing twin phylogeny and true alignment
  twin_phylogeny <- ape::read.tree(text = "(A:1, B:1);")
  beautier::check_phylogeny(twin_phylogeny)

  root_sequence <- "acgt"
  pirouette::check_root_sequence(root_sequence)

  true_alignment <- pirouette::get_test_alignment(
    n_taxa = ape::Ntip(twin_phylogeny),
    sequence_length = nchar(root_sequence)
  )
  pirouette::check_alignment(true_alignment)

  # function signature
  out <- NA
  tryCatch({
      suppressWarnings(
        out <- sim_twal_fun(
          twin_phylogeny = twin_phylogeny,
          true_alignment = true_alignment,
          root_sequence = root_sequence
        )
      )
    }, error = function(e) {
      stop(
        "'sim_twal_fun' failed to run cleanly on test input. \n",
        "Error: ", e$message
      )
    }
  )
  # function return type
  tryCatch(
    pirouette::check_alignment(out),
    condition = function(c) {
      stop(
        "'sim_twal_fun' must be a function that returns an ape::DNAbin" # nolint indeed a long string
      )
    }
  )
  testthat::expect_equal(
    pirouette::get_alignment_n_taxa(out),
    pirouette::get_alignment_n_taxa(true_alignment)
  )
  testthat::expect_equal(
    pirouette::get_alignment_sequence_length(out),
    pirouette::get_alignment_sequence_length(true_alignment)
  )
}
