#' Get the sequences from an alignment
#'
#' @inheritParams default_params_doc
#' @return a numeric vector with the sequences
#' @export
get_alignment_sequences <- function(
  alignment,
  verbose = FALSE
) {
  if (class(alignment) != "DNAbin") {
    stop("'alignment' must be of class 'ape::DNAbin'")
  }
  if (is.matrix(alignment)) {
    if (verbose) {
      print("Hey, a matrix")
    }
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
    if (verbose) {
      print("Hey, a list")
    }
    # Nah, 'byrow' really must be TRUE here
    alignment_sequences <- matrix(
      unname(unlist(as.character(alignment))),
      nrow = length(labels(alignment)),
      ncol = length(as.character(alignment)[[1]]),
      byrow = TRUE
    )
  }

  sequences <- rep(NA, get_alignment_n_taxa(alignment))

  for (i in 1:nrow(alignment_sequences)) {
    sequence_vector <- alignment_sequences[i, ]
    sequences[i] <- paste0(sequence_vector, collapse = "")
  }
  sequences
}
