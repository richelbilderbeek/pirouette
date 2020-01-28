test_that("use", {

  if (rappdirs::app_dir()$os == "win") return()

  experiments <- list()
  experiments[[1]] <- create_test_cand_experiment()
  experiments[[2]] <- experiments[[1]]

  expect_silent(check_candidates_save_to_same_files(experiments))
})

test_that("first tracelog filename is NA", {

  if (rappdirs::app_dir()$os == "win") return()

  experiments <- list()
  experiments[[1]] <- create_test_cand_experiment()
  experiments[[2]] <- experiments[[1]]
  experiments[[1]]$inference_model$mcmc$tracelog$filename <- NA

  expect_error(check_candidates_save_to_same_files(experiments))
})

test_that("tracelog filenames differ", {

  if (rappdirs::app_dir()$os == "win") return()

  experiments <- list()
  experiments[[1]] <- create_test_cand_experiment()
  experiments[[2]] <- experiments[[1]]
  experiments[[1]]$inference_model$mcmc$tracelog$filename <- "different.log"

  expect_error(check_candidates_save_to_same_files(experiments))
})

test_that("first screenlog filename is NA", {

  if (rappdirs::app_dir()$os == "win") return()

  experiments <- list()
  experiments[[1]] <- create_test_cand_experiment()
  experiments[[2]] <- experiments[[1]]
  experiments[[1]]$inference_model$mcmc$screenlog$filename <- NA

  expect_error(check_candidates_save_to_same_files(experiments))
})

test_that("screenlog filenames differ", {

  if (rappdirs::app_dir()$os == "win") return()

  experiments <- list()
  experiments[[1]] <- create_test_cand_experiment()
  experiments[[2]] <- experiments[[1]]
  experiments[[1]]$inference_model$mcmc$screenlog$filename <- "different.csv"

  expect_error(check_candidates_save_to_same_files(experiments))
})


test_that("first treelog filename is NA", {

  if (rappdirs::app_dir()$os == "win") return()

  experiments <- list()
  experiments[[1]] <- create_test_cand_experiment()
  experiments[[2]] <- experiments[[1]]
  experiments[[1]]$inference_model$mcmc$treelog$filename <- NA

  expect_error(check_candidates_save_to_same_files(experiments))
})

test_that("treelog filenames differ", {

  if (rappdirs::app_dir()$os == "win") return()

  experiments <- list()
  experiments[[1]] <- create_test_cand_experiment()
  experiments[[2]] <- experiments[[1]]
  experiments[[1]]$inference_model$mcmc$treelog$filename <- "different.trees"

  expect_error(check_candidates_save_to_same_files(experiments))
})

test_that("treelog filenames differ", {

  if (rappdirs::app_dir()$os == "win") return()

  experiments <- list()
  experiments[[1]] <- create_test_cand_experiment()
  experiments[[2]] <- experiments[[1]]
  experiments[[1]]$errors_filename <- "different.csv"

  expect_error(check_candidates_save_to_same_files(experiments))
})
