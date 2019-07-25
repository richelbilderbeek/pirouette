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
    # Nah, 'byrow' really must be TRUE here
    alignment_sequences <- matrix(
      unname(unlist(as.character(alignment))),
      nrow = length(labels(alignment)),
      ncol = length(as.character(alignment)[[1]]),
      byrow = TRUE
    )
  }

  n_taxa <- nrow(alignment_sequences)
  if (verbose) print(paste0("alignment has ", n_taxa, " taxa"))
  testit::assert(n_taxa == get_alignment_n_taxa(alignment))

  sequences <- rep(NA, n_taxa)

  for (i in seq(1, n_taxa)) {
    sequence_vector <- alignment_sequences[i, ]
    sequences[i] <- paste0(sequence_vector, collapse = "")
  }
  sequences
}
