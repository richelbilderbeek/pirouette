#' Check if the root sequence is valid
#'
#' Will \link{stop} if not
#' @inheritParams default_params_doc
#' @export
check_root_sequence <- function(root_sequence) {
  if (!pirouette::is_dna_seq(root_sequence) || length(root_sequence) != 1) {
    stop("'root_sequence' must be one lowercase DNA character string")
  }
}
