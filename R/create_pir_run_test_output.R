#' Create the same output of \link{pir_run}
#' to be used for testing
#' @author Richel J.C. Bilderbeek
#' @export
create_pir_run_test_output <- function() {

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

  df
}