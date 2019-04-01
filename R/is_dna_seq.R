#' Determine if the string is a lowercase DNA sequence
#' of at least one base pair
#' @param s the string to be checked
#' @return TRUE if the string is a lowercase DNA sequence
#'   of at least one base pair
#' @examples
#'   library(testthat)
#'
#'   # OK: valid and lowercase characters
#'   expect_true(is_dna_seq("acgt"))
#'
#'   # Must be lowercase
#'   expect_false(is_dna_seq("AGCT"))
#'
#'   # Must be only valid characters
#'   expect_false(is_dna_seq("xxxx"))
#'
#'   # Must have at least one nucleotide
#'   expect_false(is_dna_seq(""))
#' @author Richèl J.C. Bilderbeek
is_dna_seq <- function(s) {
  stringr::str_match(s, "[acgt]*")[1, 1] != ""
}
