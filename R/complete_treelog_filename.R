#' Complete a treelog's filename
#' @inheritParams default_params_doc
#' @export
complete_treelog_filename <- function(
  treelog_filename,
  fasta_filename
) {
  new_treelog_filename <- gsub(
    x = treelog_filename,
    pattern = "\\$\\(tree\\)",
    replacement = beautier::get_alignment_id(
      fasta_filename
    )
  )
  if (dirname(new_treelog_filename) != ".") {
    return(new_treelog_filename)
  }
  # Use alignment's path
  file.path(
    dirname(fasta_filename),
    basename(new_treelog_filename)
  )
}
