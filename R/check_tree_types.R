#' Checks if the tree types are valid
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @author Richèl J.C. Bilderbeek
#' @examples
#'
#' expect_silent(check_tree_types(get_tree_types()))
#' expect_error(check_tree_types("nonsense"))
#' expect_error(check_tree_types(NA))
#' expect_error(check_tree_types(NULL))
#' @export
check_tree_types <- function(tree_types) {
  if (length(tree_types) == 0) {
    stop("'tree_types' must have at least one element")
  }
  for (i in seq_along(tree_types)) {
    pirouette::check_tree_type(tree_types[i])
  }
}
