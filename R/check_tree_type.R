#' Checks if tree type is valid
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#' check_tree_type(get_tree_types()[1])
#' check_tree_type(get_tree_types()[2])
#' @export
check_tree_type <- function(
  tree_type
) {
  tree_types <- pirouette::get_tree_types()
  out <- rep(NA, length(tree_types))
  for (l in seq_along(tree_types)) {
    out[l] <- paste("'", tree_types[l], "'", sep = "")
  }
  if (!(tree_type %in% tree_types)) {
    stop(
      paste0(
        "'tree_type' must be among the following: ",
        paste(out, collapse = ", "), "."
      )
    )
  }
}
