#' Converts a phylogeny to a random DNA alignment
#' @param phylogeny a phylogeny
#' @param sequence_length the number of nucleotides to alignment
#'   will have per taxon
#' @param mutation_rate the rate per nucleotide to change,
#'   per million years
#' @return an alignment
#' @examples
#' alignment <- sim_alignment(
#'    phylogeny = ape::rcoal(5),
#'    sequence_length = 2,
#'    mutation_rate = 1
#'  )
#'  testit::assert(ribir::is_alignment(alignment))
#' @author Richel Bilderbeek
#' @export
sim_alignment <- function(
  phylogeny,
  sequence_length,
  mutation_rate = 1
) {
  if (class(phylogeny) != "phylo") {
    stop("parameter 'phylogeny' must be a phylogeny")
  }
  if (sequence_length < 1) {
    stop(
      "convert_phylogeny_to_alignment: ",
      "parameter 'sequence_length' must be ",
      "a non-zero and positive integer value"
    )
  }
  if (mutation_rate < 0) {
    stop(
      "convert_phylogeny_to_alignment: ",
      "parameter 'mutation_rate' must be a non-zero and positive value"
    )
  }
  if (!is.null(geiger::is.extinct(phylogeny))) {
    stop("phylogeny must not contain extant species")
  }

  # Jukes-Cantor 1969 model:
  #  * equal base frequencies
  #  * equal transition rates
  alignment_phydat <- phangorn::simSeq(
    phylogeny,
    l = sequence_length,
    rate = mutation_rate
  )
  testit::assert(class(alignment_phydat) == "phyDat")

  alignment_dnabin <- ape::as.DNAbin(alignment_phydat)

  testit::assert(nrow(alignment_dnabin) == length(phylogeny$tip.label))
  testit::assert(ncol(alignment_dnabin) == sequence_length)

  return(alignment_dnabin)
}
