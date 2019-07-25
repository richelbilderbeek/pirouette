plot_alignment_from_file <- function(fasta_filename) {
  if (!file.exists(fasta_filename)) {
    stop("Alignment file not found, with filename '", fasta_filename, "'")
  }
  alignment <- NULL
  tryCatch({
    alignment <- ape::read.FASTA(file = fasta_filename)
  }, warning = function(e) {
      stop(
        "Alignment file invalid. \n",
        "Error message: ", e$msg
      )
    }
  )
  testit::assert(class(alignment) == "DNAbin")
  ape::image.DNAbin(alignment)
}
