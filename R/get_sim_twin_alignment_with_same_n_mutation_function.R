#' Get a function to simulate a twin alignment from a
#' twin phylogeny and a true alignment, of which the twin
#' alignment 
#' @export
get_sim_twin_alignment_with_same_n_mutation_function <- function() {
  function(twin_phylogeny, true_alignment) {
    ape::as.DNAbin(phangorn::simSeq(twin_phylogeny))
  }
}