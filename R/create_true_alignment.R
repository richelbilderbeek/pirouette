#' Create the true alignment from the true/given phylogeny.
#'
#' We call this the true alignment, as it could thruthfully be found
#' in nature, when assuming the true phylogeny is the true
#' evolutionary history.
#' @inheritParams default_params_doc
#' @return an alignment of type \code{DNAbin}
#' @seealso Use \link{create_alignment_file} to save the created alignment
#'   directly to a file
#' @examples
#' library(testthat)
#'
#' # Create the ancestor's DNA sequence
#' n_base_pairs <- 4
#' root_sequence <- create_blocked_dna(length = n_base_pairs)
#'
#' # How to simulate the alignment
#' alignment_params <- create_alignment_params(
#'   root_sequence = root_sequence
#' )
#'
#' # Create a phylogeny to simulate the DNA sequences on
#' n_taxa <- 5
#' phylogeny <- ape::rcoal(n_taxa)
#'
#' # Simulate the alignment
#' alignment <- create_true_alignment(
#'    true_phylogeny = phylogeny,
#'    alignment_params = alignment_params
#'  )
#'
#' expect_silent(check_alignment(alignment))
#' expect_equal(nrow(alignment), n_taxa)
#' expect_equal(ncol(alignment), n_base_pairs)
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_true_alignment <- function(
  true_phylogeny,
  alignment_params
) {
  if (class(true_phylogeny) != "phylo") {
    stop(
      "'true_phylogeny' must be a valid phylogeny. \n",
      "Actual value: ", true_phylogeny
    )
  }
  pirouette::check_reconstructed_phylogeny(true_phylogeny)
  tryCatch(
    pirouette::check_alignment_params(alignment_params),
    error = function(e) {
      msg <- paste0(
        "'alignment_params' must be a set of alignment parameters. ",
        e$msg
      )
      stop(msg)
    }
  )

  sim_true_alignment(
    true_phylogeny = true_phylogeny,
    alignment_params = alignment_params
  )
}
