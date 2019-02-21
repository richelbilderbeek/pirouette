#' Estimates best lambda for a given n_taxa and n_0 and then
#'   calculates likelihood of a tree, given its branching times
#'
#' This is used to create a stunning tree
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @return the likelihood
#' @noRd
calc_likelihood_stunning <- function(
  brts,
  t_0 = 10,
  n_taxa = 5,
  n_0 = 2,
  precision = round(13 * n_taxa / 10)
) {
  cond_1 <- (length(brts) != length(unique(brts)))
  cond_2 <- (length(brts) != (n_taxa - n_0))
  cond_3 <- (any(brts < 0))
  cond_4 <- (any(brts > t_0))
  if (cond_1 | cond_2 | cond_3 | cond_4) {
    loglik <- -Inf
  } else {
    lambda <- (log(n_taxa) - log(n_0)) / t_0
    mu <- 0.0
    r <- 0.0 - (lambda - mu)
    brts2 <- c(t_0, brts, 0)
    n_s <- seq(n_0, n_taxa)
    x <- exp(n_s * r * (-diff(brts2)))
    if (precision > 0) {
      # Approximated likelihood: numerically more stable
      exponents <- 1:precision
      loglik <- sum(
        outer(X = x, Y = exponents, FUN = function(x, y) -(x ^ y) / y)
      )
    }
    if (precision == 0) {
      # Full likelihood
      loglik <- sum(log(1 - x))
    }
    if (precision < 0) {
      stop("'precision' must be positive")
    }
  }
  -loglik
}

#' Create figure bd for pirouette article
#' @inheritParams default_params_doc
#' @return a list containing a tree in phylo format, the same tree in
#' newick format and the plot of the tree
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
create_stunning_tree <- function(
  t_0 = 10,
  n_taxa = 5,
  n_0 = 2,
  precision = round(13 * n_taxa / 10)
) {
  start_brts <- seq(from = 0, to = t_0, by = t_0 / (n_taxa - n_0 + 1))
  start_brts <- sort(start_brts[2:(length(start_brts) - 1)], decreasing = TRUE)
  x <- subplex::subplex(
    par = start_brts,
    fn = calc_likelihood_stunning,
    t_0 = t_0,
    n_taxa = n_taxa,
    n_0 = n_0,
    precision = precision
  )
  brts <- sort(c(rep(t_0, n_0), x$par), decreasing = TRUE)
  brts <- signif(
    brts,
    digits = round(log(2 + n_taxa / 2))
  )
  l_table <- matrix(NA, nrow = n_taxa, ncol = 4)
  l_table[, 4] <- -1
  l_table[, 3] <- seq(from = 1, to = n_taxa) *
    cos(pi * seq(from = 0, to = n_taxa - 1))
  l_table[, 2] <- seq(from = 0, to = n_taxa - 1) *
    cos(pi * seq(from = 1, to = n_taxa))
  l_table[, 1] <- brts
  tree <- DDD::L2phylo(L = l_table)
  LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS))) # nolint
  tree$tip.label <- LETTERS702[1:n_taxa] # nolint ape::phylo does not use snake_case
  tree
}
