#' Determines if the argument is one int
#' @param x the object to be determined of if it is one integer
#' @author Richèl J.C. Bilderbeek
#' @export
is_one_int <- function(x) {
  if (length(x) != 1) return(FALSE)
  is.numeric(x)
}
