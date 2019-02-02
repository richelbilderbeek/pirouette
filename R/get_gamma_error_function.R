#' Get an error function that uses the difference in gamma statistic
#' @author Richel J.C. Bilderbeek
#' @export
get_gamma_error_function <- function() {
  gamma_error_function <- function(tree, trees) {
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
