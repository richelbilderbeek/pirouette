test_that("use", {
  expect_silent(create_twinning_params())
})

test_that("Twin tree should be BD by default, #161", {
  twinning_params <- create_twinning_params()

  tree <- ape::read.tree(text = "((A:1, B:1):1, C:2);")

  # Create using default function
  set.seed(42)
  twin_tree <- twinning_params$sim_twin_tree_fun(tree)

  # Create using BD function
  set.seed(42)
  bd_twin_tree <- create_sim_bd_twin_tree_fun()(tree)

  expect_equal(
    ape::branching.times(twin_tree),
    ape::branching.times(bd_twin_tree)
  )
})

test_that("abuse", {

  # Exact error messages checked by 'check_twinning_params'
  expect_error(
    create_twinning_params(
      rng_seed_twin_tree = "nonsense"
    ),
    "rng_seed_twin_tree"
  )
  expect_error(
    create_twinning_params(
      rng_seed_twin_alignment = "nonsense"
    ),
    "rng_seed_twin_alignment"
  )
  expect_error(
    create_twinning_params(
      twin_model = 42
    ),
    "twin_model"
  )
  expect_error(
    create_twinning_params(
      twin_model = "nonsense"
    ),
    "twin_model"
  )
  expect_error(
    create_twinning_params(
      twin_tree_filename = NA
    ),
    "twin_tree_filename"
  )
  expect_error(
    create_twinning_params(
      twin_alignment_filename = 42
    ),
    "twin_alignment_filename"
  )
})
