#' Make sure that model_select_params is a list
#' #' @inheritParams default_params_doc
#' @author Giovanni Laudanno
list_model_select_params <- function(model_select_params) {
  if (length(model_select_params) > 1) {
    listed_params <- list(model_select_params)
  } else {
    listed_params <- model_select_params
  }
  testit::assert(length(listed_params) == 1)
  testit::assert(is.list(listed_params) == 1)
  listed_params
}
