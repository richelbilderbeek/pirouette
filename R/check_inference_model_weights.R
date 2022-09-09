#' Check the one or more inference model's weights
#' @param inference_model_weight the one or more inference model's weights
#' @return nothing
#' @export
check_inference_model_weights <- function( # nolint indeed a long function name
  inference_model_weight
) {
  for (i in seq_along(inference_model_weight)) {
    weight <- inference_model_weight[i]
    if (beautier::is_one_na(weight)) next
    if (!beautier::is_one_double(weight)) {
      stop("Each 'model_weight' must be NA or a double")
    }
    if (weight < 0.0 || weight > 1.0) {
      stop("Each 'model_weight' must be a double in range [0.0, 1.0]")
    }
  }
  invisible(inference_model_weight)
}
