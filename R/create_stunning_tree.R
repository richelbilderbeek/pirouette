#' Estimates best lambda for a given n_taxa and n_0 and then
#'   calculates likelihood of a tree, given its branching times
#'
#' This is used to create a stunning tree
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno
#' @return the likelihood
#' @noRd
calc_likelihood_stunning <- function(
    brts,
    t_0 = 10,
    n_taxa = 5,
    n_0 = 2
  ) {
  cond_1 <- (length(brts) != length(unique(brts)))
  cond_2 <- (length(brts) != (n_taxa - n_0))
  cond_3 <- (any(brts < 0))
  cond_4 <- (any(brts > t_0))
  if (cond_1 | cond_2 | cond_3 | cond_4) {
    loglik <- -Inf
  } else {
    lambda <- (log(n_taxa) - log(n_0) ) / t_0
    mu <- 0
    r <- -(lambda - mu)
    lik <- (1 - exp(2 * r * (t_0 - brts[1]))) *
      (1 - exp(3 * r * (brts[1] - brts[2]))) *
      (1 - exp(4 * r * (brts[2] - brts[3]))) *
      (1 - exp(5 * r * brts[3]))
    loglik <- log(lik)
  }
  out <- -loglik
  return(out)
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
  n_0 = 2
) {
  start_brts <- seq(from = 0, to = t_0, by = t_0 / (n_taxa - n_0 + 1))
  start_brts <- sort(start_brts[2:(length(start_brts) - 1)], decreasing = TRUE)
  x <- subplex::subplex(
    par = start_brts,
    fn = calc_likelihood_stunning,
    t_0 = t_0,
    n_taxa = n_taxa,
    n_0 = n_0
  )
  brts <- sort(c(rep(t_0, n_0), x$par), decreasing = TRUE)
  brts <- signif(brts, digits = 2)
  l_table <- matrix(NA, nrow = n_taxa, ncol = 4)
  l_table[, 4] <- -1
  l_table[, 3] <- c(1, -2, 3, -4, 5)
  l_table[, 2] <- c(0, 1, 1, -2, 3)
  l_table[, 1] <- brts
  tree <- DDD::L2phylo(L = l_table)
  tree$tip.label <- LETTERS[1:n_taxa]
  tree
}
