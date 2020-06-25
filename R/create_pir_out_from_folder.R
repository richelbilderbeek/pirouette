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
    tree_prior = NA,
    error_1 = NA
  )

  t$tree <- as.factor(t$tree)
  t$inference_model <- as.factor(t$inference_model)
  t$site_model <- as.factor(t$site_model)
  t$clock_model <- as.factor(t$clock_model)
  t$tree_prior <- as.factor(t$tree_prior)

  # Inference models
  gen_xml_filename <- list.files(
    path = folder_name,
    pattern = "^gen.xml$",
    full.names = TRUE
  )
  testthat::expect_equal(1, length(gen_xml_filename))
  gen_inf_model <- xmltob2im::create_inference_model_from_file(gen_xml_filename)


  # Errors
  gen_errors_filename <- list.files(
    path = folder_name,
    pattern = "gen_errors.csv",
    full.names = TRUE
  )
  testthat::expect_equal(1, length(gen_errors_filename))
  gen_twin_errors_filename <- list.files(
    path = folder_name,
    pattern = "gen_errors_twin.csv",
    full.names = TRUE
  )
  testthat::expect_equal(1, length(gen_twin_errors_filename))
  best_errors_filename <- list.files(
    path = folder_name,
    pattern = "best_errors.csv",
    full.names = TRUE
  )
  testthat::expect_equal(1, length(best_errors_filename))
  best_twin_errors_filename <- list.files(
    path = folder_name,
    pattern = "best_errors_twin.csv",
    full.names = TRUE
  )
  testthat::expect_equal(1, length(best_twin_errors_filename))

  t
}
