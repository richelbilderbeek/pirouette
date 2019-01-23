#' Make sure that model_select_params is a list
#' #' @inheritParams default_params_doc
#' @author Giovanni Laudanno
list_model_select_params <- function(model_select_params) {
  expected_names <- names(
    create_gen_model_select_param(
      alignment_params = create_alignment_params(
        mutation_rate = 0.01
      )
    )
  )
  if (expected_names[1] %in% names(model_select_params)) {
    listed_params <- list(model_select_params)
  } else {
    listed_params <- model_select_params
  }
  testit::assert(is.list(listed_params) == 1)
  listed_params
}
