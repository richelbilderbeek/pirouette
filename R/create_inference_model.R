#' Create an inference model.
#'
#' An inference model is a combination of a site model, clock
#' model and tree prior. An inference model is also the part
#' of the BEAST2 setups that differ; the part that is shared
#' is created by \link{create_inference_params})
#' @inheritParams default_params_doc
#' @return an inference model
#' @export
#' @author Richel J.C. Bilderbeek
create_old_skool_inference_model <- function(
  site_model,
  clock_model,
  tree_prior,
  beast2_input_filename = tempfile(fileext = ".xml"),
  beast2_output_log_filename = tempfile(fileext = ".log"),
  beast2_output_trees_filename = tempfile(fileext = ".trees"),
  beast2_output_state_filename = tempfile(fileext = ".xml.state")
) {
  beautier::check_clock_model(clock_model)
  beautier::check_site_model(site_model)
  beautier::check_tree_prior(tree_prior)

  inference_model <- list(
    site_model = site_model,
    clock_model = clock_model,
    tree_prior = tree_prior,
    beast2_input_filename = beast2_input_filename,
    beast2_output_log_filename = beast2_output_log_filename,
    beast2_output_trees_filename = beast2_output_trees_filename,
    beast2_output_state_filename = beast2_output_state_filename
  )
  check_old_skool_inference_model(inference_model) # nolint pirouette function
  inference_model
}
