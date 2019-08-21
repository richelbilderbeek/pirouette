library(babette)
library(peregrine)
library(testthat)

beast2_options <- peregrine::create_pff_beast2_options()

trees <- babette::bbt_run_from_model(
  fasta_filename = beautier::get_fasta_filename(),
  beast2_options = beast2_options,
  inference_model = create_inference_model(
    mcmc = create_mcmc(chain_length = 3000)
  )
)$test_output_0_trees
expect_equal(class(trees), "multiPhylo")
expect_true(file.exists(beast2_options$output_trees_filenames))
