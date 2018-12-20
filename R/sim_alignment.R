#' Converts a phylogeny to a random DNA alignment
#' @param phylogeny a phylogeny
#' @param sequence_length the number of nucleotides to alignment
#'   will have per taxon
#' @param root_sequence the DNA sequence at the root of the phylogeny.
#'   By default, this will consist out of only adenine
#' @param mutation_rate the rate per nucleotide to change,
#'   per million years
#' @return an alignment of type \code{DNAbin}
#' @examples
#' n_taxa <- 5
#' n_base_pairs <- 10
#' alignment <- sim_alignment(
#'    phylogeny = ape::rcoal(n_taxa),
#'    alignment_params = create_alignment_params(
#'      sequence_length = n_base_pairs,
#'      mutation_rate = 1
#'    )
#'  )
#'  testit::assert(class(alignment) == "DNAbin")
#'  testit::assert(nrow(alignment) == n_taxa)
#'  testit::assert(ncol(alignment) == n_base_pairs)
#' @author Richel Bilderbeek
#' @export
sim_alignment <- function(
  phylogeny,
  alignment_params
) {
  if (class(phylogeny) != "phylo") {
    stop("parameter 'phylogeny' must be a phylogeny")
  }
  if (!is.null(geiger::is.extinct(phylogeny))) {
    stop("phylogeny must not contain extant species")
  }
  tryCatch(
    check_alignment_params(alignment_params),
    error = function(msg) {
      msg <- paste0(
        "'alignment_params' must be a set of alignment parameters. ",
        msg
      )
      stop(msg)
    }
  )

  root_sequence <- alignment_params$root_sequence


  # Jukes-Cantor 1969 model:
  #  * equal base frequencies
  #  * equal transition rates
  alignment_phydat <- phangorn::simSeq(
    phylogeny,
    l = nchar(root_sequence),
    rate = alignment_params$mutation_rate,
    rootseq = strsplit(root_sequence, split = "")[[1]]
  )
  testit::assert(class(alignment_phydat) == "phyDat")

  alignment_dnabin <- ape::as.DNAbin(alignment_phydat)

  testit::assert(nrow(alignment_dnabin) == length(phylogeny$tip.label))
  testit::assert(ncol(alignment_dnabin) == nchar(root_sequence))

  alignment_dnabin
}
