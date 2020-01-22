#' Do multiple \link{pirouette} runs
#'
#' This is a simple convenience functions: supply as much phylogenies
#' as \link{pirouette} parameter sets. For each phylogeny-parameters
#' pair, \link{pir_run} is called.
#' @inheritParams default_params_doc
#' @return a list of \link{pir_run} outputs.
#' @examples
#' if (is_on_travis() && is_beast2_installed()) {
#'
#'   pir_paramses <- list()
#'   pir_paramses[[1]] <- pirouette::create_test_pir_params()
#'   pir_paramses[[2]] <- pirouette::create_test_pir_params()
#'
#'   phylogenies <- list()
#'   phylogenies[[1]] <- ape::read.tree(text = "((A:2, B:2):1, C:3);")
#'   phylogenies[[2]] <- ape::read.tree(text = "((A:1, B:1):2, C:3);")
#'
#'   pir_outs <- pir_runs(
#'     phylogenies = phylogenies,
#'     pir_paramses = pir_paramses
#'   )
#'   for (pir_out in pir_outs) {
#'     expect_silent(check_pir_out(pir_out))
#'   }
#' }
#' @seealso Use \link{pir_run} for a single \link{pirouette} run.
#' @author Richèl J.C. Bilderbeek
#' @export
pir_runs <- function(
  phylogenies,
  pir_paramses
) {
  testit::assert(length(phylogenies) == length(pir_paramses))
  n_runs <- length(phylogenies)

  pir_outs <- list()

  for (i in seq_along(n_runs)) {
    pir_outs[[i]] <- pir_run(
      phylogeny = phylogenies[[i]],
      pir_params = pir_paramses[[i]]
    )
  }
  pir_outs
}
