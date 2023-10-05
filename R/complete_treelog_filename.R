#' Complete a treelog's filename
#' @inheritParams default_params_doc
#' @return the filename for the treelog
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # Check cleanup by other functions
#' beastier::check_empty_beaustier_folders()
#'
#' fasta_filename <- "/home/alignment_folder/my.fasta"
#'
#' # Will become '/home/alignment_folder/my.trees'
#' complete_treelog_filename(
#'   treelog_filename = "$(tree).trees",
#'   fasta_filename = fasta_filename
#' )
#'
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
