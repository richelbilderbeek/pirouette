#' Create a \code{pir_out} from a folder
#'
#' @inheritParams default_params_doc
#' @return a \code{pir_out}, as can be checked by \link{check_pir_out}
#' @author Richèl J.C. Bilderbeek
#' @export
create_pir_out_from_folder <- function(
  folder_name
) {
  if (!dir.exists(folder_name)) {
    stop("'folder_name' does not exist. Actual value: ", folder_name)
  }
  t <- tidyr::expand_grid(
    tree = c("true", "twin"),
    inference_model = c("generative", "candidate"),
    inference_model_weight = NA,
    site_model = NA,
    clock_model = NA,
    tree_prior = NA
  )

  t$tree <- as.factor(t$tree)
  t$inference_model <- as.factor(t$inference_model)


  # Inference models
  xml_filenames <- c(
    list.files(path = folder_name, pattern = "^gen.xml$", full.names = TRUE),
    list.files(path = folder_name, pattern = "^best.xml$", full.names = TRUE),
    list.files(path = folder_name, pattern = "^gen_twin.xml$", full.names = TRUE), # nolint indeed a long line
    list.files(path = folder_name, pattern = "^best_twin.xml$", full.names = TRUE) # nolint indeed a long line
  )
  testthat::expect_equal(4, length(xml_filenames))

  inference_models <- tiebeaur::create_inference_models_from_files(
    xml_filenames
  )
  testthat::expect_equal(4, length(inference_models))
  for (i in seq(1, 4)) {
    t$site_model[i] <- inference_models[[i]]$site_model$name
    t$clock_model[i] <- inference_models[[i]]$clock_model$name
    t$tree_prior[i] <- inference_models[[i]]$tree_prior$name
  }
  # Errors
  errors_filenames <- c(
    list.files(path = folder_name, pattern = "^gen_errors.csv$", full.names = TRUE), # nolint indeed a long line
    list.files(path = folder_name, pattern = "^best_errors.csv$", full.names = TRUE), # nolint indeed a long line
    list.files(path = folder_name, pattern = "^gen_errors_twin.csv$", full.names = TRUE), # nolint indeed a long line
    list.files(path = folder_name, pattern = "^best_errors_twin.csv$", full.names = TRUE) # nolint indeed a long line
  )
  testthat::expect_equal(4, length(errors_filenames))

  n_errors <- nrow(readr::read_csv(errors_filenames[1]))
  df_errors <- data.frame(matrix(nrow = 4, ncol = n_errors, data = 0.0))
  colnames(df_errors) <- paste0("error_", 1:n_errors)
  t_errors <- tibble::as_tibble(df_errors)

  for (i in seq_len(4)) {
    t_errors[i, 1:n_errors] <- t(readr::read_csv(errors_filenames[i])$x)
  }

  t <- cbind(t, t_errors)


  t$site_model <- as.factor(t$site_model)
  t$clock_model <- as.factor(t$clock_model)
  t$tree_prior <- as.factor(t$tree_prior)
  t
}
