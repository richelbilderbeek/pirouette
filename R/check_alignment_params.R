#' Checks if the argument is a valid alignment parameters structure,
#' as created by \link{create_alignment_params}.
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @author Richèl J.C. Bilderbeek
#' @examples
#'
#' expect_silent(check_alignment_params(create_test_alignment_params()))
#' expect_error(check_alignment_params("nonsense"))
#' expect_error(check_alignment_params(NA))
#' expect_error(check_alignment_params(NULL))
#' @export
check_alignment_params <- function(
  alignment_params
) {
  pirouette::check_alignment_params_names(alignment_params)
  pirouette::check_root_sequence(alignment_params$root_sequence)

  if (!beautier::is_one_int(alignment_params$rng_seed)) {
    stop("'rng_seed' must be a number")
  }
}

#' Checks if the list elements' names match that
#' of a valid \code{alignment_params}
#'
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @author Richèl J.C. Bilderbeek
#' @export
check_alignment_params_names <- function(
  alignment_params
) {
  argument_names <- c(
    "root_sequence",
    "sim_tral_fun",
    "rng_seed",
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
}
