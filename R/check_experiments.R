#' Checks if the argument is a list of one or more \link{pirouette} experiments.
#'
#' Will \link{stop} if not.
#' A valid \link{pirouette} experiment
#' can be created by \link{create_experiment}.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @seealso Use \link{check_experiment} to check if an object
#'   is one valid experiment
#' @examples
#'   library(testthat)
#'
#'   expect_silent(check_experiments(list(create_test_experiment())))
#'   expect_error(check_experiments(create_test_experiment()))
#'   expect_error(check_experiments("nonsense"))
#'   expect_error(check_experiments(NA))
#'   expect_error(check_experiments(NULL))
#' @author Richèl J.C. Bilderbeek
#' @export
check_experiments <- function(
  experiments
) {
  if (!is.list(experiments)) {
    stop(
      "'experiments' must be a list of one or more experiments.\n",
      "Actual value: ", experiments
    )
  }
  for (i in seq_along(experiments)) {
    experiment <- experiments[[i]]
    tryCatch(
      check_experiment(experiment), # nolint pirouette function
      error = function(e) {
        stop(
          "'experiments[[", i, "]] invalid.\n",
          "Tip: use 'create_experiment' for each list element.\n",
          "Error: ", e$message, "\n",
          "Value: ", experiment
        )
      }
    )
  }
  if (length(experiments) == 1) return()

  for (i in seq(1, length(experiments) - 1)) {
    experiment_1 <- experiments[[i]]
    for (j in seq(i + 1, length(experiments))) {
      testit::assert(j > i)
      experiment_2 <- experiments[[j]]
      if (
        !are_equal_mcmcs(
          experiment_1$inference_model$mcmc,
          experiment_2$inference_model$mcmc
        )
      ) {
        stop(
          "All MCMCs in the experiments must be identical.\n",
          "Difference between experiment[[", i, "]] ",
          "and experiment[[", j, "]].\n",
          "Value experiment[[", i, "]]$inference_model$mcmc: ",
          paste0(experiments[[i]]$inference_model$mcmc, collapse = ", "), "\n",
          "Value experiment[[", j, "]]$inference_model$mcmc: ",
          paste0(experiments[[j]]$inference_model$mcmc, collapse = ", "), "\n"
        )
      }
    }
  }

  model_types <- rep("", length(experiments))
  for (i in 1:length(experiments)) {
    model_types[i] <- experiments[[i]]$inference_conditions$model_type
  }
  if (sum(model_types == "generative") > 1) {
    stop("Specifying more than one 'generative' model experiment is redundant")
  }
  if (length(experiments) > 1) {
    exp_types <- rep(NA, length(experiments))
    for (i in seq_along(experiments)) {
      exp_types[i] <- experiments[[i]]$inference_conditions$model_type
    }
    if (exp_types[1] != "generative" && ("generative" %in% exp_types)) {
      stop("If multiple experiments, generative is either first or absent")
    }
  }
}
