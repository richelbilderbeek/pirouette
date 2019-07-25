test_that("use", {
  fasta_filename <- system.file(
    "extdata", "alignment.fas", package = "pirouette"
  )
  expect_silent(plot_alignment_from_file(fasta_filename))
})

test_that("abuse", {
  expect_error(
    plot_alignment_from_file("nonsense"),
    "Alignment file not found"
  )

  filename <- tempfile()
  writeLines(text = "nonsense", con = filename)
  expect_error(
    plot_alignment_from_file(filename),
    "Alignment file invalid"
  )
})
