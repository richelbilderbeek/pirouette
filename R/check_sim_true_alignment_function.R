#' Check if the function to create a true alignment with (from the
#' true phylogeny) is valid.
#'
#' Will \link{stop} if not
#' @inheritParams default_params_doc
#' @seealso Use \link{check_sim_twin_alignment_function} to check a function to
#' generate a twin alignment
#' @export
check_sim_true_alignment_function <- function(sim_true_alignment_function) {
  if (!is.function(sim_true_alignment_function)) {
    stop("'sim_true_alignment_function' must be a function")
  }
  # sim_true_alignment_function must return a DNAbin
  out <- NA
  tryCatch({
      out <- sim_true_alignment_function(
        true_phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"))
    }, error = function(e) {
      stop(
        "Error when using 'sim_true_alignment_function' on an example ",
        "phylogeny. \n",
        "Error message: ", e$message
      )
    }
  )
  if (class(out) != "DNAbin") {
    stop(
      "'sim_true_alignment_function' must be a function that returns",
      " an ape::DNAbin. \n",
      "Actual class returned: ", class(out)
    )
  }

}
