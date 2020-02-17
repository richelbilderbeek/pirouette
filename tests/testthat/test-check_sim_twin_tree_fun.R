test_that("use", {

  expect_silent(
    check_sim_twin_tree_fun(
      get_sim_bd_twin_tree_fun()
    )
  )

  expect_silent(
    check_sim_twin_tree_fun(
      get_sim_yule_twin_tree_fun()
    )
  )
})


test_that("abuse", {
  expect_error(
    check_sim_twin_tree_fun(
      sim_twin_tree_fun = "nonsense"
    ),
    "'sim_twin_tree_fun' must be a function"
  )

  expect_error(
    check_sim_twin_tree_fun(
      sim_twin_tree_fun = function(one_too, many_arguments) { } # nolint ignore curly braces placement here
    ),
    "'sim_twin_tree_fun' must be a function with one argument"
  )

  expect_error(
    check_sim_twin_tree_fun(
      sim_twin_tree_fun = function(invalid_argument = "irrelevant") {
        ape::rcoal(3)
      }
    ),
    paste0(
      "'sim_twin_tree_fun' must be a function with one argument",
      " called 'true_phylogeny'"
    )
  )

  expect_error(
    check_sim_twin_tree_fun(
      sim_twin_tree_fun = function(true_phylogeny = "irrelevant") {
        "not a phylo"
      }
    ),
    "'sim_twin_tree_fun' must be a function that returns an ape::phylo"
  )

  true_phylogeny <-
    DDD:::dd_sim(pars = c(0.4, 0.1, Inf), age = 3, ddmodel = 1)$tes
  expect_error(
    check_sim_twin_tree_fun(
      sim_twin_tree_fun = function(true_phylogeny) {
        true_phylogeny2 <- true_phylogeny
        true_phylogeny2$tip.label <- paste0(true_phylogeny$tip.label, "h") # nolint ape uses dots in variabele names
        true_phylogeny2
      }
    ),
    "'sim_twin_tree_fun' must return a tree with the same taxon labels"
  )

  expect_error(
    check_sim_twin_tree_fun(
      sim_twin_tree_fun = function(true_phylogeny) {
        phytools::bind.tip(tree = true_phylogeny, tip.label = "t_new")
      }
    ),
    "'sim_twin_tree_fun' must return a tree with the same number of taxa"
  )

  expect_error(
    check_sim_twin_tree_fun(
      sim_twin_tree_fun = function(
        true_phylogeny = "irrelevant"
      ) {
        ape::rtree(10) # A non-ultrametric tree
      }
    ),
    "'sim_twin_tree_fun' must return an ultrametric tree"
  )

})
