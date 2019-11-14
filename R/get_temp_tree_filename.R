#' Get the name for a temporary file to store a tree
#' in Newick format
#' @export
get_temp_tree_filename <- function() {
  tempfile(
    pattern = "tree_",
    tmpdir = rappdirs::user_cache_dir(),
    fileext = ".newick"
  )
}
