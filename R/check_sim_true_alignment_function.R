#' Check if the function to create a true alignment with (from the
#' true phylogeny) is valid.
#'
#' Will \link{stop} if not
#' @inheritParams default_params_doc
#' @export
check_sim_true_alignment_function <- function(sim_true_alignment_function) {
  if (!is.function(sim_true_alignment_function)) {
    stop("'sim_true_alignment_function' must be a function")
  }
  # check if sim_true_alignment_function is indeed a function with 1 parameter
  arguments <- utils::capture.output(
    utils::str(args(sim_true_alignment_function))
  )
  if (stringr::str_count(string = arguments, pattern = ",") > 0) {
    stop(
      "'sim_true_alignment_function' must be a function with one argument"
    )
  }
  # sim_true_alignment_function must return a DNAbin
  if (class(
    sim_true_alignment_function(
      ape::read.tree(text = "((A:1, B:1):1, C:2);"))
    ) != "DNAbin"
  ) {
    stop(
      "'sim_true_alignment_function' must be a function that returns an ape::DNAbin"
    )
  }

}
