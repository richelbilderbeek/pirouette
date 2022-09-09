#' Check if a twin phylogeny is a valid phylogeny
#' @seealso Use \link[beautier]{check_phylogeny}
#' for checking phylogenies in general
#' @inheritParams default_params_doc
#' @return nothing
#' @author Richèl J.C. Bilderbeek
#' @examples
#' phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
#' check_twin_phylogeny(phylogeny)
#' @export
check_twin_phylogeny <- function(twin_phylogeny) {
  tryCatch(
    beautier::check_phylogeny(twin_phylogeny),
    error = function(e) { # nolint indeed ignore e
      stop(
        "'twin_phylogeny' must be a valid phylogeny.\n",
        "Actual value: ", twin_phylogeny
      )
    }
  )
  invisible(twin_phylogeny)
}
