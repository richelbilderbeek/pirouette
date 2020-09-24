#' Get an error function that uses the difference in gamma statistic.
#' @note the gamma statistic can be found in Pybus and Harvey, 2000,
#' 'Testing macro-evolutionary models using incomplete molecular phylogenies.',
#' Proc. R. Soc. Lond. B, 267, 2267-2272.
#' @author Richèl J.C. Bilderbeek
#' @examples
#'
#' error_fun <- get_gamma_error_fun()
#'
#' phylogeny <- ape::read.tree(text = "((A:1.5, B:1.5):1.5, C:3.0);")
#'
#' tree_1 <- ape::read.tree(text = "((A:1.0, B:1.0):2.0, C:3.0);")
#' tree_2 <- ape::read.tree(text = "((A:2.0, B:2.0):1.0, C:3.0);")
#' trees <- c(tree_1, tree_2)
#'
#' lowest_error <- error_fun(phylogeny, c(phylogeny))
#' error_1 <- error_fun(phylogeny, c(tree_1))
#' error_2 <- error_fun(phylogeny, c(tree_2))
#' expect_true(lowest_error < error_1)
#' expect_true(lowest_error < error_2)
#' expect_equal(2, length(error_fun(phylogeny, trees)))
#' @export
get_gamma_error_fun <- function() {
  gamma_error_fun <- function(tree, trees) {
    errors <- rep(NA, length(trees))
    given_gamma <- phytools::gammatest(
      phytools::ltt(tree, plot = FALSE, gamma = FALSE)
    )$gamma
    for (i in seq_along(trees)) {
      one_tree <- trees[[i]]
      this_gamma <- phytools::gammatest(
        phytools::ltt(one_tree, plot = FALSE, gamma = FALSE)
      )$gamma
      errors[i] <- abs(given_gamma - this_gamma)
    }
    errors
  }
}
