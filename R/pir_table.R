#' Displays the results of \link{pir_run} as a table
#' @inheritParams default_params_doc
#' @return the results of \link{pir_run} as a table
#' @seealso
#'   Use \link{pir_plot} to display the output of \link{pir_run} as a
#'   figure.
#' @author Richel J.C. Bilderbeek
#' @export
pir_table <- function(run_experiments) {
  check_run_experiments(run_experiments) # nolint pirouette function

  # Count the number of rows needed
  n_rows <- 0
  for (run_experiment in run_experiments) {
    if (!is_one_na(run_experiment$true_result)) n_rows <- n_rows + 1
    if (!is_one_na(run_experiment$twin_result)) n_rows <- n_rows + 1
  }

  # Prepare data frame
  df <- data.frame(
    tree = rep(NA, n_rows),
    inference_model = rep(NA, n_rows),
    inference_model_weight = rep(NA, n_rows),
    site_model = rep(NA, n_rows),
    clock_model = rep(NA, n_rows),
    tree_prior = rep(NA, n_rows)
  )

  # Find the highest number of errors measured
  max_n_errors <- 0
  for (run_experiment in run_experiments) {
    if (!is_one_na(run_experiment$true_result) &&
      !is_one_na(run_experiment$true_result$errors)
    ) {
      max_n_errors <- max(
        length(run_experiment$true_result$errors), max_n_errors
      )
    }
    if (!is_one_na(run_experiment$twin_result) &&
      !is_one_na(run_experiment$twin_result$errors)
    ) {
      max_n_errors <- max(
        length(run_experiment$twin_result$errors), max_n_errors
      )
    }
  }

  error_col_names <- paste0("error_", seq(1, max_n_errors))
  df[, error_col_names] <- NA

  for (i in seq_along(run_experiments)) {
    run_experiment <- run_experiments[[i]]
    check_run_experiment(run_experiment) # nolint pirouette function

    # True
    if (!is_one_na(run_experiment$true_result)) {
      result <- run_experiment$true_result
      errors <- result$errors
      df$tree[i] <- "true"
      df$inference_model[i] <- run_experiment$inference_conditions$model_type
      df$inference_model_weight[i] <- result$weight
      df$site_model[i] <- run_experiment$inference_model$site_model$name
      df$clock_model[i] <- run_experiment$inference_model$clock_model$name
      df$tree_prior[i] <- run_experiment$inference_model$tree_prior$name
      from_col_idx <- which(colnames(df) == "error_1")
      to_col_idx <- from_col_idx - 1 + length(errors)
      df[i, from_col_idx:to_col_idx] <- errors
      #df$log_evidence[i] <- result$log_evidence
      #df$weight[i] <- result$weight
      i <- i + 1
    }
    # Twin
    if (!is_one_na(run_experiment$twin_result)) {
      result <- run_experiment$twin_result
      errors <- result$errors
      df$tree[i] <- "twin"
      df$inference_model[i] <- run_experiment$inference_conditions$model_type
      df$inference_model_weight[i] <- result$weight
      df$site_model[i] <- run_experiment$inference_model$site_model$name
      df$clock_model[i] <- run_experiment$inference_model$clock_model$name
      df$tree_prior[i] <- run_experiment$inference_model$tree_prior$name
      from_col_idx <- which(colnames(df) == "error_1")
      to_col_idx <- from_col_idx - 1 + length(errors)
      df[i, from_col_idx:to_col_idx] <- errors
      #df$log_evidence[i] <- result$log_evidence
      #df$weight[i] <- result$weight
      i <- i + 1
    }
  }

  df$tree <- as.factor(df$tree)
  df$inference_model <- as.factor(df$inference_model)
  df$site_model <- as.factor(df$site_model)
  df$clock_model <- as.factor(df$clock_model)
  df$tree_prior <- as.factor(df$tree_prior)

  df
}
