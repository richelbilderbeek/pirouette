#' Check the model type names
#' @param model_type_names one or more model type names, each
#'   element must equal a value in \link{get_model_type_names}
#' @return nothing
#' @export
check_inference_model_type_names <- function(model_type_names) { # nolint indeed a long name
  if (!all(model_type_names %in% get_model_type_names())) {
    stop("Invalid 'inference_model' value")
  }
  invisible(model_type_names)
}
