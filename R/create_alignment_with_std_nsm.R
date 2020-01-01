#' Create an alignment with a standard site model
#' @inheritParams default_params_doc
#' @return an alignment of type \code{DNAbin}
#' @examples
#' phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
#' alignment_params <- create_alignment_params(
#'   root_sequence = "aaaa",
#'   sim_tral_fun = get_sim_tral_with_std_nsm_fun(
#'     mutation_rate = 0.1
#'   )
#' )
#' alignment <- create_alignment_with_std_nsm(
#'   phylogeny = phylogeny,
#'   alignment_params = alignment_params
#' )
#'
#' expect_silent(check_alignment(alignment))
#' @export
create_alignment_with_std_nsm <- function(
  phylogeny,
  alignment_params
) {
  mutation_rate <- get(
    "mutation_rate",
    environment(alignment_params$sim_tral_fun)
  )
  site_model <- get(
    "site_model",
    environment(alignment_params$sim_tral_fun)
  )
  pirouette::check_mutation_rate(mutation_rate)
  pirouette::check_site_model(site_model)
  pirouette::sim_alignment_with_std_nsm(
    phylogeny = phylogeny,
    root_sequence = alignment_params$root_sequence,
    mutation_rate = mutation_rate,
    site_model = site_model
  )
}
