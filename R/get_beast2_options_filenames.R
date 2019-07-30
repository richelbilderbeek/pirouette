#' Extract the filenames from a \code{beast2_options}
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @export
get_beast2_options_filenames <- function(beast2_options) {
  beastier::check_beast2_options(beast2_options)
  c(
    beast2_options$input_filename,
    beast2_options$output_log_filename,
    beast2_options$output_trees_filenames,
    beast2_options$output_state_filename
  )
}
