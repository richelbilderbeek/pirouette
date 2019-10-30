#' Get a default function to simulate a twin alignment from a
#' twin phylogeny and a true alignment
#' @export
get_default_sim_twin_alignment_function <- function() {
  function(twin_phylogeny, true_alignment) {
    ape::as.DNAbin(phangorn::simSeq(twin_phylogeny))
  }
}
