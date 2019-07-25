context("test-count_n_mutations")

test_that("use, single taxon", {

  #
  # Root sequence is known:AAAA
  #
  # AAAA -> AAGG                                                                # nolint this is no commented code
  #
  # Those are two mutations
  #
  # Don't forget: ape assumes lowercase
  root_sequence <- "aaaa"
  alignment <- ape::as.DNAbin(
    x = list(species_1 = strsplit("aagg", split = "")[[1]])
  )
  get_alignment_sequence_length(alignment)
  ape::image.DNAbin(alignment)
  expect_equal(
    count_n_mutations(alignment = alignment, root_sequence = root_sequence),
    2
  )
})

test_that("use, two taxa", {

  #
  # Root sequence is known
  #
  #           +---- AAAACCCC 4 mutations
  # AAAAAAAA -+
  #           +---- AAAATTTT 4 mutations
  #                          ----------- +
  #                          8 mutations
  #
  # Those are eight mutations in total
  #
  # Don't forget: ape assumes lowercase
  root_sequence <- "aaaaaaaa"
  alignment <- ape::as.DNAbin(x = list(
      species_1 = strsplit("aaaacccc", split = "")[[1]],
      species_2 = strsplit("aaaatttt", split = "")[[1]]
    )
  )
  ape::image.DNAbin(alignment)
  expect_equal(
    count_n_mutations(alignment = alignment, root_sequence = root_sequence),
    8
  )
})


test_that("use, two taxa with 9 nucleotides", {

  #
  # Root sequence is known
  #
  #            +---- AAAACCCCG 5 mutations
  # AAAAAAAAC -+
  #            +---- AAAATTTTT 5 mutations
  #                            ----------- +
  #                           10 mutations
  #
  # Those are eight mutations in total
  #
  # Don't forget: ape assumes lowercase
  root_sequence <- "aaaaaaaac"
  alignment <- ape::as.DNAbin(x = list(
      species_1 = strsplit("aaaaccccg", split = "")[[1]],
      species_2 = strsplit("aaaattttt", split = "")[[1]]
    )
  )
  ape::image.DNAbin(alignment)
  expect_equal(
    count_n_mutations(alignment = alignment, root_sequence = root_sequence),
    10
  )
})

test_that("use, two taxa with 9 nucleotides, no mutation", {

  #
  # Root sequence is known
  #
  #            +---- AAAAAAAAA 0 mutations
  # AAAAAAAAC -+
  #            +---- AAAAAAAAA 0 mutations
  #                            ----------- +
  #                            0 mutations
  #
  # Those are eight mutations in total
  #
  # Don't forget: ape assumes lowercase
  root_sequence <- "aaaaaaaaa"
  alignment <- ape::as.DNAbin(x = list(
      species_1 = strsplit("aaaaaaaaa", split = "")[[1]],
      species_2 = strsplit("aaaaaaaaa", split = "")[[1]]
    )
  )
  ape::image.DNAbin(alignment)
  expect_equal(
    count_n_mutations(alignment = alignment, root_sequence = root_sequence),
    0
  )
})

test_that("use, two taxa with 9 nucleotides, from FASTA", {

  #
  # Root sequence is known
  #
  #            +---- AAAACCCCG 5 mutations
  # AAAAAAAAC -+
  #            +---- AAAATTTTT 5 mutations
  #                            ----------- +
  #                           10 mutations
  #
  # Those are eight mutations in total
  #
  # Don't forget: ape assumes lowercase
  fasta_filename <- tempfile()
  writeLines(
    text = c(">X", "AAAACCCCG", ">Y", "AAAATTTTT"),
    con = fasta_filename
  )
  root_sequence <- "aaaaaaaac"
  alignment <- ape::read.FASTA(fasta_filename)
  ape::image.DNAbin(alignment)
  expect_equal(
    count_n_mutations(alignment = alignment, root_sequence = root_sequence),
    10
  )
})

test_that("use, three taxa", {

  #
  # Root sequence is known
  #
  #           +---- AAAACCCC 4 mutations
  # AAAAAAAA -+
  #           | +-- AAAATTTT 4 mutations
  #           +-+
  #             +-- AAAATTGG 4 mutations
  #                          ----------- +
  #                         12 mutations
  #
  # Those are twelve mutations in total
  #
  # Don't forget: ape assumes lowercase
  root_sequence <- "aaaaaaaa"
  alignment <- ape::as.DNAbin(x = list(
      species_1 = strsplit("aaaacccc", split = "")[[1]],
      species_2 = strsplit("aaaatttt", split = "")[[1]],
      species_3 = strsplit("aaaattgg", split = "")[[1]]
    )
  )
  ape::image.DNAbin(alignment)
  expect_equal(
    count_n_mutations(alignment = alignment, root_sequence = root_sequence),
    12
  )
})




test_that("use, three taxa, bug", {

  skip("Issue 298, Issue #298")
  #
  # Root sequence is known
  #
  #        +------- AGGCA 2 mutations
  # AGCTA -+          ^^
  #        |    +-- AGCCA 1 mutations
  #        +----+      ^
  #             +-- AACTA 1 mutations
  #                  ^      ----------- +
  #                       4 mutations
  #
  # Those are four mutations in total
  #
  # Don't forget: ape assumes lowercase
  root_sequence <- "agcta"
  alignment <- ape::as.DNAbin(x = list(
      species_1 = strsplit("aggca", split = "")[[1]],
      species_2 = strsplit("agcca", split = "")[[1]],
      species_3 = strsplit("aacta", split = "")[[1]]
    )
  )
  ape::image.DNAbin(alignment, main = root_sequence, show.bases = TRUE)
  expect_equal(
    count_n_mutations(alignment = alignment, root_sequence = root_sequence),
    4
  )
})


test_that("abuse", {

  expect_error(
    count_n_mutations(
      alignment = "nonsense",
      root_sequence = "aaaa"
    ),
    "'alignment' must be of class 'ape::DNAbin'"
  )

  expect_error(
    count_n_mutations(
      alignment = ape::as.DNAbin(
        x = list(species_1 = strsplit("aaaaaaaa", split = "")[[1]])
      ),
      root_sequence = "nonsense"
    ),
    "'root_sequence' must be one character vector of lowercase nucleotides"
  )

  root_sequence <- "aaaa"
  alignment <- ape::as.DNAbin(
    x = list(species_1 = strsplit("aaaaaaaaaaaaaaaaaaaaaaaaa", split = "")[[1]])
  )
  expect_error(
    count_n_mutations(
      alignment = alignment,
      root_sequence = root_sequence
    ),
    "'root_sequence' must have the same length as each taxon's sequence length"
  )
})

test_that("Simplified version of bug #269", {

  true_tree <- ape::read.tree(text = "((A:1, B:1):2, C:3);")
  root_sequence <- create_blocked_dna(8)
  alignment <- create_true_alignment(
    true_phylogeny = true_tree,
    alignment_params = create_test_alignment_params(
      root_sequence = root_sequence,
      mutation_rate = 0.0
    )
  )
  image(alignment)
  expect_equal(
    0,
    count_n_mutations(
      alignment = alignment, root_sequence = root_sequence
    )
  )
})

test_that("Bug #269, no mutations for mutation rate zero", {

  # Thanks to @thijsjanzen for finding and sharing this bug
  sequence_length <- 40
  # Simulated the Newick using:
  #
  # phy <- TESS::tess.sim.taxa.age(n = 1, nTaxa = 10, age = 1, lambda = 1, mu = 0)[[1]] # nolint indeed this is code
  #
  phy <- ape::read.tree(text = "((t2:0.8025722798,t7:0.8025722798):0.1974277202,(((t4:0.3850106179,t9:0.3850106179):0.4928072344,t6:0.8778178522):0.04975597904,((t1:0.4535164265,(t8:0.04947219126,t5:0.04947219126):0.4040442352):0.224021363,(t3:0.2922007622,t10:0.2922007622):0.3853370273):0.2500360418):0.07242616874);") # nolint indeed a long line
  root_sequence <- pirouette::create_blocked_dna(length = sequence_length)
  root_sequence_for_phangorn <- strsplit(root_sequence, split = "")[[1]]
  alignment_phydat <- phangorn::simSeq(
    x = phy,
    l = sequence_length,
    rootseq = root_sequence_for_phangorn,
    rate = 0.0
  )
  alignment_dnabin <- ape::as.DNAbin(alignment_phydat)
  alignment <- alignment_dnabin
  testit::assert(get_alignment_sequence_length(alignment) == sequence_length)
  image(alignment_dnabin)
  expect_equal(
    0,
    count_n_mutations(alignment = alignment, root_sequence = root_sequence)
  )
})
