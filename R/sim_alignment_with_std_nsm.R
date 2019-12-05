#' Create an alignment with a standard site model using a raw interface
#' @inheritParams default_params_doc
#' @return an alignment of type \code{DNAbin}
#' @examples
#' library(testthat)
#'
#' alignment <- sim_alignment_with_std_nsm(
#'   phylogeny = ape::read.tree(text = "((A:1, B:1):2, C:3);"),
#'   root_sequence = "aaaa",
#'   mutation_rate = 0.1,
#'   site_model = beautier::create_jc69_site_model()
#' )
#' expect_silent(check_alignment(alignment))
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
  if (class(alignment_phydat) != "phyDat") {
    stop(
      "'class(alignment_phydat)' not equal to 'phyDat'. \n",
      "Actual 'class(alignment_phydat)': ", class(alignment_phydat), " \n",
      "Actual 'alignment_phydat': ", alignment_phydat, " \n"
    )
  }
  testit::assert(class(alignment_phydat) == "phyDat")

  alignment <- ape::as.DNAbin(alignment_phydat)
  pirouette::check_alignment(alignment)
  testit::assert(
    pirouette::get_alignment_sequence_length(alignment) ==
    nchar(root_sequence)
  )
  testit::assert(
    pirouette::get_alignment_n_taxa(alignment) ==
    ape::Ntip(phylogeny)
  )

  alignment
}