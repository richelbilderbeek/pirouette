#' Create the same output of \link{pir_run}
#' to be used for testing
#' @param add_twin add rows for twin tree
#' @param add_best add rows for best inference model
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   pir_plot(
#'     create_test_pir_run_output(
#'       add_twin = TRUE,
#'       add_best = TRUE
#'     )
#'   )
#' @export
create_test_pir_run_output <- function(
  add_twin = FALSE,
  add_best = FALSE
) {

  df <- data.frame(tree = c("true", "true", "twin", "twin"))

  df$inference_model <- c("generative", "best", "generative", "best")
  df$inference_model_weight <- c(0.5, 0.4, 0.6, 0.3)
  df$site_model <- c("JC69", "HKY", "JC69", "GTR")
  df$clock_model <- c("strict", "strict", "strict", "RLN")
  df$tree_prior <- c("BD", "Yule", "BD", "CCP")
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
