#' Adapter function to create
#' a twin alignment by simply copying the
#' true alignment
#' @inheritParams default_params_doc
#' @return the true alignment
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @examples
#' # Check cleanup by other functions
#' beastier::check_empty_beaustier_folders()
#'
#' true_alignment <- get_test_alignment()
#' twin_alignment <- copy_true_alignment(true_alignment)
#'
#' # twin_alignment equals true_alignment
#' @export
copy_true_alignment <- function(
  true_alignment,
  twin_phylogeny = "irrelevant",
  root_sequence = "irrelevant"
) {
  true_alignment
}
