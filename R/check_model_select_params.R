#' Checks if the argument is a valid list of one or more
#' model selection parameter sets.
#'
#' Will \link{stop} if not.
#' Valid model selection parameter sets can be created
#' by \link{create_model_select_param}.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if the object is not
#' @author Richel J.C. Bilderbeek
check_model_select_params <- function(
  model_select_params
) {
  if (!is.list(model_select_params)) {
    stop("'model_select_params' must be a list")
  }
  for (model_select_param in model_select_params) {
    check_model_select_param(model_select_param) # nolint pirouette function
  }
}
