#' Get an error function that uses the nLTT statistic
#' @author Richel J.C. Bilderbeek
#' @export
get_nltt_error_function <- function() {
  nLTT::nltts_diff
}