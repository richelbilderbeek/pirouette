test_that("use", {

  fasta_filename <- "/home/alignment_folder/my.fasta"

  # Local file must get path of FASTA file
  expect_equal(
    complete_treelog_filename(
      treelog_filename = "$(tree).trees",
      fasta_filename = fasta_filename
    ),
    "/home/alignment_folder/my.trees"
  )
  # Path of treelog must remain
  expect_equal(
    complete_treelog_filename(
      treelog_filename = "/home/other_folder/$(tree).trees",
      fasta_filename = fasta_filename
    ),
    "/home/other_folder/my.trees"
  )
  # Path of treelog must remain, also when matching the FASTA file
  expect_equal(
    complete_treelog_filename(
      treelog_filename = "/home/alignment_folder/$(tree).trees",
      fasta_filename = fasta_filename
    ),
    "/home/alignment_folder/my.trees"
  )
})
