#' Check if the inference parameters are valid.
#' Throws if these are invalid
#' @author Richel J.C. Bilderbeek
check_inference_params <- function(
  inference_params
) {
  if (!"rng_seed" %in% inference_params) {

  }
  rng_seed <- inference_params$rng_seed
  if (rng_seed <= 0) {
    stop("'rng_seed' should be NA or non-zero positive")
  }
}
