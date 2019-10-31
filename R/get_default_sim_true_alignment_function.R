#' Get a default function to simulate a 'true' alignment from a 'true' phylogeny
#' @export
get_default_sim_true_alignment_function <- function() {
  function(true_phylogeny) {
    ape::as.DNAbin(phangorn::simSeq(true_phylogeny))
  }
}
