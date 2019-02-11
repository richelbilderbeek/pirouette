#' Is the experiment the one with the most evidence?
#'
#' @inheritParams default_params_doc
#' @export
is_best_candidate <- function(experiment, marg_liks) {

  if (nrow(marg_liks) == 0) return(FALSE)

  testit::assert(
    all(
      c("weight", "site_model_name", "clock_model_name", "tree_prior_name")
      %in% names(marg_liks)
    )
  )
  best_row_index <- which(marg_liks$weight == max(marg_liks$weight))
  best_site_model_name <- marg_liks$site_model_name[best_row_index]
  best_clock_model_name <- marg_liks$clock_model_name[best_row_index]
  best_tree_prior_name <- marg_liks$tree_prior_name[best_row_index]

  site_model_name <- experiment$inference_model$site_model$name
  clock_model_name <- experiment$inference_model$clock_model$name
  tree_prior_name <- experiment$inference_model$tree_prior$name

  best_site_model_name == site_model_name &&
    best_clock_model_name == clock_model_name &&
    best_tree_prior_name == tree_prior_name
}
