#' Check if the function to create a twin alignment with (from a
#' true phylogeny and a true alignment) is valid.
#'
#' Will \link{stop} if not
#' @inheritParams default_params_doc
#' @seealso
#' Use \link{check_sim_true_alignment_function} to check a function to
#' generate a true alignment.
#' Use \link{check_sim_twin_tree_function} to check a function to
#' generate a twin phylogeny.
#' @export
check_sim_twin_alignment_function <- function(sim_twin_alignment_function) {
  if (!is.function(sim_twin_alignment_function)) {
    stop("'sim_twin_alignment_function' must be a function")
  }
  # check if sim_twin_alignment_function is indeed a function with 2 parameters
  arguments <- utils::capture.output(
    utils::str(args(sim_twin_alignment_function))
  )
  # n_args <- stringr::str_count(string = arguments, pattern = ",") + 1
  # if (n_args != 2) {
  #   stop(
  #     "'sim_twin_alignment_function' must be a function with two arguments. \n",
  #     "Actual value: ", n_args, "\n",
  #     "arguments: '", arguments, "'"
  #   )
  # }

  # sim_twin_alignment_function must return a DNAbin
  # Use a simple testing twin phylogeny and true alignment
  twin_phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  beautier::check_phylogeny(twin_phylogeny)

  root_sequence <- "aaaa"
  pirouette::check_root_sequence(root_sequence)

  true_alignment <- get_test_alignment(
    n_taxa = ape::Ntip(twin_phylogeny),
    sequence_length = nchar(root_sequence)
  )
  pirouette::check_alignment(true_alignment)

  # function signature
  out <- NA
  tryCatch({
      out <- sim_twin_alignment_function(
        twin_phylogeny = twin_phylogeny,
        true_alignment = true_alignment,
        root_sequence = root_sequence
      )
    }, error = function(e) {
      stop(
        "'sim_twin_alignment_function' failed to run cleanly on test input. \n",
        "Error: ", e$message
      )
    }
  )
  # function return type
  tryCatch(
    check_alignment(out),
    condition = function(c) {
      stop(
        "'sim_twin_alignment_function' must be a function that returns an ape::DNAbin"
      )
    }
  )
}
