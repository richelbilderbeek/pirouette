#' Get a default function to simulate an alignment from a phylogeny
#' @export
get_default_sim_alignment_function <- function() {
  function(phylogeny) {
    ape::as.DNAbin(phangorn::simSeq(phylogeny))
  }
}