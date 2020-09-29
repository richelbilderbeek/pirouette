#' Convert the collect of errors to a data frame
#'
#' @inheritParams default_params_doc
#' @param errorses a collection of errors (hence the reduplicated plural)
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @examples
#'
#' if (beastier::is_beast2_installed()) {
#'   phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
#'   pir_params <- create_test_pir_params()
#'
#'   # A normal user should not need to call 'phylo_to_errors' directly.
#'   # For a developer that needs to, the 'pir_params' must be initialized
#'   pir_params <- init_pir_params(pir_params)
#'
#'   create_tral_file(
#'     phylogeny = phylogeny,
#'     alignment_params = pir_params$alignment_params,
#'     verbose = FALSE
#'   )
#'
#'   errorses <- list()
#'   errorses[[1]] <- phylo_to_errors(
#'     phylogeny = phylogeny,
#'     alignment_params = pir_params$alignment_params,
#'     error_measure_params = pir_params$error_measure_params,
#'     experiment = pir_params$experiments[[1]],
#'     verbose = pir_params$verbose
#'   )
#'
#'   errorses_to_data_frame(
#'     errorses = errorses,
#'     experiments = list(pir_params$experiments[[1]]),
#'     marg_liks = create_test_marg_liks(
#'       site_models = list(create_jc69_site_model()),
#'       clock_models = list(create_strict_clock_model()),
#'       tree_priors = list(create_yule_tree_prior())
#'     )
#'   )
#' }
#' @export
errorses_to_data_frame <- function(
  errorses,
  experiments,
  marg_liks
) {
  pirouette::check_experiments(experiments)
  testit::assert(length(errorses) > 0)
  testit::assert(length(experiments) == length(errorses))
  if (length(errorses) > 1) {
    if (length(errorses[[1]]) != length(errorses[[2]])) {
      stop(
        "Lengths between errorses differ (", length(errorses[[1]]),
        " vs ", length(errorses[[2]]), ")."
      )
    }
    testit::assert(length(errorses[[1]]) == length(errorses[[2]]))
  }

  # Put inference models and errors a data frame
  n_rows <- length(experiments)
  df <- data.frame(
    tree = rep(NA, n_rows),
    inference_model = rep(NA, n_rows),
    inference_model_weight = rep(NA, n_rows),
    site_model = rep(NA, n_rows),
    clock_model = rep(NA, n_rows),
    tree_prior = rep(NA, n_rows)
  )

  error_col_names <- paste0("error_", seq(1, length(errorses[[1]])))
  df[, error_col_names] <- NA

  for (i in seq_along(experiments)) {
    experiment <- experiments[[i]]
    nltts <- errorses[[i]]
    pirouette::check_experiment(experiment)
    df$inference_model[i] <- experiment$inference_conditions$model_type
    df$inference_model_weight[i] <- NA
    df$site_model[i] <- experiment$inference_model$site_model$name
    df$clock_model[i] <- experiment$inference_model$clock_model$name
    df$tree_prior[i] <- experiment$inference_model$tree_prior$name
    from_col_idx <- which(colnames(df) == "error_1")
    df[i, from_col_idx:ncol(df)] <- nltts
  }

  # Add evidence (marginal likelihoods) in columns
  if (!is.null(marg_liks)) {
    for (i in seq_along(experiments)) {
      experiment <- experiments[[i]]
      marg_liks_row <- which(
        marg_liks$site_model_name ==
          experiment$inference_model$site_model$name &
        marg_liks$clock_model_name ==
          experiment$inference_model$clock_model$name &
        marg_liks$tree_prior_name ==
          experiment$inference_model$tree_prior$name
      )
      # if there is no row, 'which' returns a zero-length vector
      # Happens when the generative model is not part of the models
      # under selection
      if (length(marg_liks_row) != 0) {
        df$inference_model_weight[i] <- marg_liks$weight[marg_liks_row]
      }
    }
  }

  df$inference_model <- as.factor(df$inference_model)
  df$site_model <- as.factor(df$site_model)
  df$clock_model <- as.factor(df$clock_model)
  df$tree_prior <- as.factor(df$tree_prior)
  df
}
