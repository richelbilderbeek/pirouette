#' Plot an alignment stored as a file
#'
#' @inheritParams default_params_doc
#' @param title the plot title
#' alignment_params <- create_test_alignment_params()
#' alignment <- create_true_alignment_file(
#'   phylogeny = ape::rcoal(4),
#'   alignment_params = alignment_params
#' )
#' plot_alignment_from_file(fasta_filename = alignment_params$fasta_filename)
#' @export
plot_alignment_from_file <- function(
  fasta_filename,
  title = ""
) {
  if (!file.exists(fasta_filename)) {
    stop("Alignment file not found, with filename '", fasta_filename, "'")
  }
  alignment <- NULL
  tryCatch({
      alignment <- ape::read.FASTA(file = fasta_filename)
    },
    warning = function(e) {
      stop(
        "Alignment file invalid. \n",
        "Error message: ", e$msg
      )
    }
  )
  testit::assert(class(alignment) == "DNAbin")
  ape::image.DNAbin(alignment, main = title, show.bases = TRUE)
}
