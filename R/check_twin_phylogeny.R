#' Check if a twin phylogeny is a valid phylogeny
#' @seealso Use \link[beautier]{check_phylogeny}
#' for checking phylogenies in general
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
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
}
