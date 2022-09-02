#' Create an alignment with a standard site model using a raw interface
#' @inheritParams default_params_doc
#' @return an alignment of type \code{DNAbin}
#' @examples
#'
#' alignment <- sim_alignment_with_std_nsm(
#'   phylogeny = ape::read.tree(text = "((A:1, B:1):2, C:3);"),
#'   root_sequence = "aaaa",
#'   mutation_rate = 0.1,
#'   site_model = beautier::create_jc69_site_model()
#' )
#' check_alignment(alignment)
#' @seealso use \link{sim_alignment_with_std_nsm_from_params}
#' to simulate an alignment from an \code{alignment_params}, as created
#' by \link{create_alignment_params}.
#' @author Richèl J.C. Bilderbeek
#' @export
sim_alignment_with_std_nsm <- function(
  phylogeny,
  root_sequence,
  mutation_rate,
  site_model
) {
  beautier::check_phylogeny(phylogeny)
  pirouette::check_root_sequence(root_sequence)
  pirouette::check_mutation_rate(mutation_rate)
  beautier::check_site_model(site_model)
  alignment_phydat <- phangorn::simSeq(
    phylogeny,
    l = nchar(root_sequence),
    rate = mutation_rate,
    rootseq = strsplit(root_sequence, split = "")[[1]],
    Q = create_rate_matrix(
      site_model = site_model,
      base_frequencies = calc_base_freq(root_sequence)
    )
  )
  testthat::expect_equal(class(alignment_phydat), "phyDat")

  alignment <- ape::as.DNAbin(alignment_phydat)
  pirouette::check_alignment(alignment)
  testthat::expect_true(
    pirouette::get_alignment_sequence_length(alignment) ==
    nchar(root_sequence)
  )
  testthat::expect_true(
    pirouette::get_alignment_n_taxa(alignment) ==
    ape::Ntip(phylogeny)
  )

  alignment
}
