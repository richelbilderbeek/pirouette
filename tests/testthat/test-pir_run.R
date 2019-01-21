context("pir_run")

test_that("generative only", {

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params(
    mutation_rate = 0.01
  )
  file.remove(alignment_params$fasta_filename)

  errors <- pir_run(
    phylogeny = phylogeny,
    alignment_params = alignment_params,
    model_select_params = list(
      create_gen_model_select_param(
        alignment_params = alignment_params
      )
    ),
    inference_param = create_inference_param(
      mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
    )
  )
  # Files created
  testit::assert(file.exists(alignment_params$fasta_filename))

  # Return value
  expect_true("tree" %in% names(errors))
  expect_true(is.factor(errors$tree))
  expect_true("true" %in% errors$tree)

  expect_true("inference_model" %in% names(errors))
  expect_true(is.factor(errors$inference_model))
  expect_true("generative" %in% errors$inference_model)

  expect_true("inference_model_weight" %in% names(errors))
  expect_true(is.na(errors$inference_model_weight))
  expect_true(!is.factor(errors$inference_model_weight))

  expect_true("site_model" %in% names(errors))
  expect_true(is.factor(errors$site_model))
  expect_true("JC69" %in% errors$site_model)

  expect_true("clock_model" %in% names(errors))
  expect_true(is.factor(errors$clock_model))
  expect_true("strict" %in% errors$clock_model)

  expect_true("tree_prior" %in% names(errors))
  expect_true(is.factor(errors$tree_prior))
  expect_true("birth_death" %in% errors$tree_prior)

  expect_true("error_1" %in% names(errors))
  expect_true(!is.factor(errors$error_1))

  # Errors more than zero
  col_first_error <- which(colnames(errors) == "error_1")
  col_last_error <- ncol(errors)
  expect_true(all(errors[, col_first_error:col_last_error] > 0.0))

  expect_true("input_filename" %in% names(errors))
  expect_true("log_filename" %in% names(errors))
  expect_true("trees_filename" %in% names(errors))
  expect_true("state_filename" %in% names(errors))
  expect_false(is.na(errors$input_filename))
  expect_false(is.na(errors$log_filename))
  expect_false(is.na(errors$trees_filename))
  expect_false(is.na(errors$state_filename))
})

test_that("most_evidence", {

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  model_select_params <- list(
    create_best_model_select_param(
      site_models = beautier::create_site_models()[4],
      clock_models = beautier::create_clock_models()[2],
      tree_priors = beautier::create_tree_priors()[5],
      epsilon = 100.0
    )
  )
  file.remove(model_select_params[[1]]$marg_lik_filename)
  testit::assert(!file.exists(model_select_params[[1]]$marg_lik_filename))

  errors <- pir_run(
    phylogeny = phylogeny,
    alignment_params = create_alignment_params(
      root_sequence = "acgt",
      mutation_rate = 0.01
    ),
    model_select_params = model_select_params,
    inference_param = create_inference_param(
      mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
    )
  )
  expect_true("most_evidence" %in% errors$inference_model)
  expect_true(all(errors$inference_model_weight > 0.0))

  # Errors more than zero
  col_first_error <- which(colnames(errors) == "error_1")
  col_last_error <- ncol(errors)
  expect_true(all(errors[, col_first_error:col_last_error] > 0.0))

  expect_true(file.exists(model_select_params[[1]]$marg_lik_filename))
})

test_that("generative and most_evidence, generative not in most_evidence", {

  alignment_params <- create_alignment_params(mutation_rate = 0.01)

  errors <- pir_run(
    phylogeny = ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);"),
    alignment_params = alignment_params,
    model_select_params = list(
      create_gen_model_select_param(
        alignment_params = alignment_params
      ),
      create_best_model_select_param(
        site_models = beautier::create_site_models()[4],
        clock_models = beautier::create_clock_models()[2],
        tree_priors = beautier::create_tree_priors()[5]
      )
    ),
    inference_param = create_inference_param(
      mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
    )
  )
  expect_true("most_evidence" %in% errors$inference_model)
  expect_true(is.na(errors$inference_model_weight[1]))
  expect_true(is.numeric(errors$inference_model_weight[2]))

  # Errors more than zero
  col_first_error <- which(colnames(errors) == "error_1")
  col_last_error <- ncol(errors)
  expect_true(all(errors[, col_first_error:col_last_error] > 0.0))
})

test_that("generative and most_evidence, generative in most_evidence", {

  alignment_params <- create_alignment_params(mutation_rate = 0.01)

  errors <- pir_run(
    phylogeny = ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);"),
    alignment_params = alignment_params,
    model_select_params = list(
      create_gen_model_select_param(
        alignment_params = alignment_params
      ),
      create_best_model_select_param(
        site_models = list(alignment_params$site_model),
        clock_models = list(alignment_params$clock_model),
        tree_priors = list(beautier::create_bd_tree_prior())
      )
    ),
    inference_param = create_inference_param(
      mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
    )
  )
  expect_true("most_evidence" %in% errors$inference_model)
  expect_true(is.numeric(errors$inference_model_weight[1]))

  # Errors more than zero
  col_first_error <- which(colnames(errors) == "error_1")
  col_last_error <- ncol(errors)
  expect_true(all(errors[, col_first_error:col_last_error] > 0.0))

})

test_that("generative with twin", {

  if (!beastier::is_on_travis()) return()

  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
  alignment_params <- create_alignment_params(
    mutation_rate = 0.01
  )
  twinning_params <- create_twinning_params()

  # Remove files, just to be sure
  file.remove(twinning_params$twin_tree_filename)
  file.remove(twinning_params$twin_alignment_filename)

  errors <- pir_run(
    phylogeny = phylogeny,
    twinning_params = twinning_params,
    alignment_params = alignment_params,
    model_select_params = list(
      create_gen_model_select_param(
        alignment_params = alignment_params
      )
    ),
    inference_param = create_inference_param(
      mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
    )
  )

  # Files created
  testit::assert(file.exists(twinning_params$twin_tree_filename))
  testit::assert(file.exists(twinning_params$twin_alignment_filename))

  # Return value
  expect_true("tree" %in% names(errors))
  expect_true(is.factor(errors$tree))
  expect_true("true" %in% errors$tree)
  expect_true("twin" %in% errors$tree)
})
