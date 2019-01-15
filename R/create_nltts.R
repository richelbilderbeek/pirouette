#' Create the NLTT statistics distribution files from a posterior file.
#' Assumes for a posterior file named '1x.trees'
#'   that there is a 'true' tree file called '1x.tree'
#' @inheritParams default_params_doc
#' @return nltt statistics
#' @author Richel J.C. Bilderbeek
#' @export
create_nltts <- function(
  phylogeny,
  posterior
) {
  posterior_trees <- posterior$trees
  nltt_diffs <- rep(NA, length(posterior_trees))
  for (i in 1:length(posterior_trees)) {
    nltt_diffs[i] <- nLTT::nLTTstat(phylogeny, posterior_trees[[i]]) # nolint nLTT used older coding standard
  }
  nltt_diffs
}
