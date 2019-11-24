test_that("use", {

  testthat::expect_silent(
    pirouette::check_sim_twin_tree_fun(
      pirouette::create_sim_bd_twin_tree_fun()
    )
  )
})


test_that("abuse", {
  testthat::expect_error(
    pirouette::check_sim_twin_tree_fun(
      sim_twin_tree_fun = "nonsense"
    ),
    "'sim_twin_tree_fun' must be a function"
  )

  testthat::expect_error(
    pirouette::check_sim_twin_tree_fun(
      sim_twin_tree_fun = function(one_too, many_arguments) { } # nolint ignore curly braces placement here
    ),
    "'sim_twin_tree_fun' must be a function with one argument"
  )

  testthat::expect_error(
    pirouette::check_sim_twin_tree_fun(
      sim_twin_tree_fun = function(invalid_argument = "irrelevant") {
        ape::rcoal(3)
      }
    ),
    paste0(
      "'sim_twin_tree_fun' must be a function with one argument",
      " called 'true_phylogeny'"
    )
  )

  testthat::expect_error(
    pirouette::check_sim_twin_tree_fun(
      sim_twin_tree_fun = function(true_phylogeny = "irrelevant") {
        "not a phylo"
      }
    ),
    "'sim_twin_tree_fun' must be a function that returns an ape::phylo"
  )

  true_phylogeny <-
    DDD:::dd_sim(pars = c(0.4, 0.1, Inf), age = 3, ddmodel = 1)$tes
  testthat::expect_error(
    pirouette::check_sim_twin_tree_fun(
      sim_twin_tree_fun = function(true_phylogeny) {
        true_phylogeny2 <- true_phylogeny
        true_phylogeny2$tip.label <- paste0(true_phylogeny$tip.label, "h")
        true_phylogeny2
      }
    ),
    "'sim_twin_tree_fun' must return a tree with the same taxon labels"
  )

  testthat::expect_error(
    pirouette::check_sim_twin_tree_fun(
      sim_twin_tree_fun = function(true_phylogeny) {
        phytools::bind.tip(tree = true_phylogeny, tip.label = "t_new")
      }
    ),
    "'sim_twin_tree_fun' must return a tree with the same number of taxa"
  )

  testthat::expect_error(
    pirouette::check_sim_twin_tree_fun(
      sim_twin_tree_fun = function(
        true_phylogeny = "irrelevant"
      ) {
        ape::rtree(10) # A non-ultrametric tree
      }
    ),
    "'sim_twin_tree_fun' must return an ultrametric tree"
  )

})
