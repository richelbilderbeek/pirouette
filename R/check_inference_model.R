#' Check if \code{x} is a valid inference model
#'
#' Calls \link{stop} if \code{x} is not a valid inference model,
#' will do nothing otherwise
#' @param x object to be checked to be an inference model,
#'   as can be created by \link{create_old_skool_inference_model}
#' @return nothing.
#'   Calls \link{stop} if \code{x} is not a valid inference model
#' @author Richel J.C. Bilderbeek
check_old_skool_inference_model <- function( # nolint indeed a long line, luckily this is temporary
  x
) {
  stop("'check_old_skool_inference_model' deprecated")
  argument_names <- c(
    "site_model", "clock_model", "tree_prior", "beast2_input_filename",
    "beast2_output_log_filename",
    "beast2_output_trees_filename", "beast2_output_state_filename"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(x)) {
      stop(
        "'", arg_name, "' must be an element of an 'inference_model'"
      )
    }
  }
  beautier::check_clock_model(x$clock_model)
  beautier::check_site_model(x$site_model)
  beautier::check_tree_prior(x$tree_prior)
}
