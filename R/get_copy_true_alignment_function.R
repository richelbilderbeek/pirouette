#' Get a function to 'simulate' a twin alignment by simply
#' copying the true alignment.
#' @inheritParams default_params_doc
#' @return the function
#' \link{copy_true_alignment}
#' @examples
#' library(testthat)
#'
#' f <- get_copy_true_alignment_function()
#' # This adapter function must be a sim_twin_alignment function
#' expect_silent(
#'   check_sim_twin_alignment_function(f)
#' )
#' @seealso
#' See \link{check_sim_twin_alignment_function} to the the other
#' functions to simulate a twin alignment.
#' Use \link{sim_twin_alignment} to use this function to
#' create a twin alignment.
#' @export
get_copy_true_alignment_function <- function() {
  pryr::partial(
    copy_true_alignment
  )
}
