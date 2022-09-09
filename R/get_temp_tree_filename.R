#' Get the name for a temporary file to store a tree
#' in Newick format
#' @return one string
#' @export
get_temp_tree_filename <- function() {
  beautier::get_beautier_tempfilename(
    pattern = "tree_",
    fileext = ".newick"
  )
}
