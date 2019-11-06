#' Adapter function to create
#' a twin alignment by simply copying the
#' true alignment
#' @inheritParams default_params_doc
#' @export
copy_true_alignment <- function(
  true_alignment,
  twin_phylogeny = "irrelevant",
  root_sequence = "irrelevant"
) {
  true_alignment
}
