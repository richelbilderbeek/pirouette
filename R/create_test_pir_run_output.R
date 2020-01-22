#' Create the same output of \link{pir_run}
#' to be used for testing
#' @param add_twin add rows for twin tree
#' @param add_best add rows for best inference model
#' @return a data frame with errors, with as many rows as model selection
#' parameter sets. The output can be checked using \link{check_pir_out}.
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' expect_silent(
#'   check_pir_out(
#'     create_test_pir_run_output(
#'       add_twin = TRUE,
#'       add_best = TRUE
#'     )
#'   )
#' )
#'
#' pir_plot(
#'   create_test_pir_run_output(
#'     add_twin = TRUE,
#'     add_best = TRUE
#'   )
#' )
#' @export
create_test_pir_run_output <- function(
  add_twin = FALSE,
  add_best = FALSE
) {

  df <- data.frame(tree = c("true", "true", "twin", "twin"))

  df$inference_model <- rep(pirouette::get_model_type_names(), times = 2)
  df$inference_model_weight <- c(0.5, 0.4, 0.6, 0.3)
  df$site_model <- beautier::get_site_model_names()[1:4]
  df$clock_model <- rep(beautier::get_clock_model_names()[1:2], 2)
  df$tree_prior <- beautier::get_tree_prior_names()[1:4]
  df$error_1 <- c(0.1, 0.2, 0.3, 0.4)
  df$error_2 <- c(0.11, 0.23, 0.35, 0.47)
  df$error_3 <- c(0.12, 0.24, 0.36, 0.48)

  df$tree <- as.factor(df$tree)
  df$inference_model <- as.factor(df$inference_model)
  df$site_model <- as.factor(df$site_model)
  df$clock_model <- as.factor(df$clock_model)
  df$tree_prior <- as.factor(df$tree_prior)

  rows <- seq(1, 4)
  if (!add_twin) rows <- rows[c(-3, -4)]
  if (!add_best) rows <- rows[c(-2, -4)]
  df[rows, ]
}

#' Create the same output of \link{pir_run}
#' to be used for testing, but with more data
#' @param add_twin add rows for twin tree
#' @param add_best add rows for best inference model
#' @param n_errors number of errors in the pir_out
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#'   pir_plot(
#'     create_test_pir_run_output2(
#'       add_twin = TRUE,
#'       add_best = TRUE,
#'       n_errors = 1e2
#'     )
#'   )
#' @export
create_test_pir_run_output2 <- function(
  add_twin = FALSE,
  add_best = FALSE,
  n_errors = 2e2
) {

  df <- data.frame(tree = c("true", "true", "twin", "twin"))

  df$inference_model <- c("generative", "candidate", "generative", "candidate")
  df$inference_model_weight <- c(0.5, 0.4, 0.6, 0.3)
  df$site_model <- beautier::get_site_model_names()[1:4]
  df$clock_model <- rep(beautier::get_clock_model_names()[1:2], 2)
  df$tree_prior <- beautier::get_tree_prior_names()[1:4]

  dist1 <- stats::rnorm(n = n_errors, mean = 0.14, sd = 0.021)
  dist2 <- stats::rnorm(n = n_errors, mean = 0.21, sd = 0.032)
  dist3 <- stats::rnorm(n = n_errors, mean = 0.18, sd = 0.033)
  dist4 <- stats::rnorm(n = n_errors, mean = 0.25, sd = 0.041)

  df$tree <- as.factor(df$tree)
  df$inference_model <- as.factor(df$inference_model)
  df$site_model <- as.factor(df$site_model)
  df$clock_model <- as.factor(df$clock_model)
  df$tree_prior <- as.factor(df$tree_prior)
  df2 <- rbind(dist1, dist2, dist3, dist4)
  colnames(df2) <- paste0("error_", 1:n_errors)

  df3 <- cbind(df, df2)
  rownames(df3) <- NULL

  rows <- seq(1, 4)
  if (!add_twin) rows <- rows[c(-3, -4)]
  if (!add_best) rows <- rows[c(-2, -4)]
  df3[rows, ]
}
