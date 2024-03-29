test_that("must create file", {
  alignment_params <- create_test_alignment_params(
    root_sequence = "agcta"
  )
  twinning_params <- create_twinning_params(
    sim_twal_fun =
      get_sim_twal_same_n_muts_fun(
        max_n_tries = 1000
      )
  )
  true_phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  twin_phylogeny <- create_twin_tree(
    phylogeny = true_phylogeny,
    twinning_params = twinning_params
  )
  create_tral_file(
    phylogeny = true_phylogeny,
    alignment_params = alignment_params
  )
  expect_true(file.exists(alignment_params$fasta_filename))
  expect_false(file.exists(twinning_params$twin_alignment_filename))
  create_twal_file(
    twin_phylogeny = twin_phylogeny,
    alignment_params = alignment_params,
    twinning_params =  twinning_params
  )
  expect_true(file.exists(alignment_params$fasta_filename))
  expect_true(file.exists(twinning_params$twin_alignment_filename))

  expect_s3_class(
    ape::read.FASTA(alignment_params$fasta_filename),
    "DNAbin"
  )

  # Files must contain equally much mutations
  true_alignment_filename <- alignment_params$fasta_filename
  twin_alignment_filename <- twinning_params$twin_alignment_filename
  true_alignment <- ape::read.FASTA(true_alignment_filename)
  twin_alignment <- ape::read.FASTA(twin_alignment_filename)
  n_mutations_true <- count_n_mutations(
    alignment = true_alignment,
    root_sequence = alignment_params$root_sequence
  )
  n_mutations_twin <- count_n_mutations(
    alignment = twin_alignment,
    root_sequence = alignment_params$root_sequence
  )
  expect_equal(n_mutations_true, n_mutations_twin)
})
