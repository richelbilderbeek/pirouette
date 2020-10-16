#' Determine if the string is a lowercase DNA sequence
#' of at least one base pair
#' @param s the string to be checked
#' @return TRUE if the string is a lowercase DNA sequence
#'   of at least one base pair
#' @author Richèl J.C. Bilderbeek
#' @examples
#'
#' # TRUE: valid and lowercase characters
#' is_dna_seq("acgt")
#'
#' # FALSE: Must be lowercase
#' is_dna_seq("AGCT")
#'
#' # FALSE: Must be only valid characters
#' is_dna_seq("xxxx")
#'
#' # FALSE: Must have at least one nucleotide
#' is_dna_seq("")
#' @export
is_dna_seq <- function(s) {
  stringr::str_match(s, "[acgt]*")[1, 1] != ""
}
