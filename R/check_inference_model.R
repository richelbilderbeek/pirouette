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
}
