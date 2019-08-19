#' @author Richèl J.C. Bilderbeek
#' @noRd
check_experiments_all_inference_models_are_unique <- function(
  experiments
) {
  testit::assert(length(experiments) >= 2)
  for (i in seq(1, length(experiments) - 1)) {

    site_model_name_1 <- experiments[[i]]$inference_model$site_model$name
    clock_model_name_1 <- experiments[[i]]$inference_model$clock_model$name
    tree_prior_name_1 <- experiments[[i]]$inference_model$tree_prior$name
    for (j in seq(i + 1, length(experiments))) {
      testit::assert(j > i)
      testit::assert(j <= length(experiments))
      site_model_name_2 <- experiments[[j]]$inference_model$site_model$name
      clock_model_name_2 <- experiments[[j]]$inference_model$clock_model$name
      tree_prior_name_2 <- experiments[[j]]$inference_model$tree_prior$name
      if (site_model_name_1  == site_model_name_2 &&
          clock_model_name_1 == clock_model_name_2 &&
          tree_prior_name_1  == tree_prior_name_2
      ) {
        stop(
          "All inference models must be unique\n",
          "Duplicate between experiment[[", i, "]] ",
          "and experiment[[", j, "]].\n"
        )
      }
    }
  }
}
