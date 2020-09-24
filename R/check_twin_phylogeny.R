#' Check if a twin phylogeny is a valid phylogeny
#' @seealso Use \link[beautier]{check_phylogeny}
#' for checking phylogenies in general
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @examples
#'
#' phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
#' expect_silent(check_twin_phylogeny(phylogeny))
#' expect_error(check_twin_phylogeny("nonsense"))
#' expect_error(check_twin_phylogeny(NULL))
#' expect_error(check_twin_phylogeny(NA))
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
