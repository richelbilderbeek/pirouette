#' Determine if the string is a lowercase DNA sequence
#' of at least one base pair
#' @param s the string to be checked
#' @return TRUE if the string is a lowercase DNA sequence
#'   of at least one base pair
#' @examples
#'   testit::assert(pirouette:::is_dna_seq("acgt"))
#'   testit::assert(!pirouette:::is_dna_seq("AGCT"))
#'   testit::assert(!pirouette:::is_dna_seq("xxxx"))
#'   testit::assert(!pirouette:::is_dna_seq(""))
#' @author Richel J.C. Bilderbeek
is_dna_seq <- function(s) {
  stringr::str_match(s, "[acgt]*")[1, 1] != ""
}
