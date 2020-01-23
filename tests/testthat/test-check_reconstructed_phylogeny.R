test_that("use", {

  expect_silent(check_reconstructed_phylogeny(ape::rcoal(4)))

  set.seed(42)
  p_with_extant <- ape::rtree(5)
  testit::assert(geiger::is.extinct(p_with_extant))
  #ape::plot.phylo(p_with_extant)
  #ape::plot.phylo(ape::drop.fossil(p_with_extant))
  expect_error(
    check_reconstructed_phylogeny(p_with_extant),
    "A reconstructed phylogeny must not contain extinct species"
  )
})
