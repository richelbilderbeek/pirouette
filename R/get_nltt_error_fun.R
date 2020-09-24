#' Get an error function that uses the nLTT statistic
#' @author Richèl J.C. Bilderbeek
#' @examples
#'
#' error_fun <- get_nltt_error_fun()
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
get_nltt_error_fun <- function() {
  nLTT::nltts_diff
}
