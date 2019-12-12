#' Get the sequences from an alignment
#'
#' @inheritParams default_params_doc
#' @return a numeric vector with the sequences
#' @examples
#' get_alignment_sequences(
#'   alignment = ape::as.DNAbin(
#'     x = list(species_1 = strsplit("aaaa", split = "")[[1]])
#'   )
#' )
#' @export
get_alignment_sequences <- function(
  alignment,
  verbose = FALSE
) {
  pirouette::check_alignment(alignment)
  if (is.matrix(alignment)) {
    if (verbose) print("alignment is a matrix")
    # We know from
    # https://github.com/richelbilderbeek/pirouette/commit/a96ec3fef34c79e38bed292092c0370e5312c3f6 # nolint indeed long
    # that byrow must be FALSE
    alignment_sequences <- matrix(
      unname(unlist(as.character(alignment))),
      nrow = length(labels(alignment)),
      ncol = ncol(alignment),
      byrow = FALSE
    )
  }
  if (is.list(alignment)) {
    if (verbose) print("alignment is a list")
    n_taxa <- length(labels(alignment))
    testit::assert(n_taxa > 0)
    # Nah, 'byrow' really must be TRUE here
    alignment_sequences <- matrix(
      unname(unlist(as.character(alignment))),
      nrow = n_taxa,
      ncol = length(as.character(alignment)[[1]]),
      byrow = TRUE
    )
  }

  n_taxa <- nrow(alignment_sequences)
  if (verbose) print(paste0("alignment has ", n_taxa, " taxa"))

  sequences <- rep(NA, n_taxa)

  for (i in seq(1, n_taxa)) {
    sequence_vector <- alignment_sequences[i, ]
    sequences[i] <- paste0(sequence_vector, collapse = "")
  }
  sequences
}
