#' Converts a phylogeny to a random DNA alignment
#' @param phylogeny a phylogeny
#' @param sequence_length the number of nucleotides to alignment
#'   will have per taxon
#' @param root_sequence the DNA sequence at the root of the phylogeny.
#'   By default, this will consist out of only adenine
#' @param mutation_rate the rate per nucleotide to change,
#'   per million years
#' @return an alignment
#' @examples
#' n_taxa <- 5
#' n_base_pairs <- 10
#' alignment <- sim_alignment(
#'    phylogeny = ape::rcoal(n_taxa),
#'    sequence_length = n_base_pairs,
#'    mutation_rate = 1
#'  )
#'  testit::assert(class(alignment) == "DNAbin")
#'  testit::assert(nrow(alignment) == n_taxa)
#'  testit::assert(ncol(alignment) == n_base_pairs)
#' @author Richel Bilderbeek
#' @export
sim_alignment <- function(
  phylogeny,
  sequence_length, #useless space
  root_sequence = rep("a", sequence_length),
  mutation_rate = 1
) {
  if (class(phylogeny) != "phylo") {
    stop("parameter 'phylogeny' must be a phylogeny")
  }
  if (sequence_length < 1) {
    stop("'sequence_length' must be a non-zero and positive integer value")
  }
  if (mutation_rate < 0) {
    stop("parameter 'mutation_rate' must be a non-zero and positive value")
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
    rate = mutation_rate,
    rootseq = root_sequence
  )
  testit::assert(class(alignment_phydat) == "phyDat")

  alignment_dnabin <- ape::as.DNAbin(alignment_phydat)

  testit::assert(nrow(alignment_dnabin) == length(phylogeny$tip.label))
  testit::assert(ncol(alignment_dnabin) == sequence_length)

  alignment_dnabin
}
