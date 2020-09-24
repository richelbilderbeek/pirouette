#' Get a function to 'simulate' a twin alignment by simply
#' copying the true alignment.
#' @return the function
#' \link{copy_true_alignment}
#' @examples
#'
#' f <- get_copy_tral_fun()
#' # This adapter function must be a sim_twin_alignment function
#' expect_silent(
#'   check_sim_twal_fun(f)
#' )
#' @seealso
#' See \link{check_sim_twal_fun} to the the other
#' functions to simulate a twin alignment.
#' Use \link{sim_twin_alignment} to use this function to
#' create a twin alignment.
#' @export
get_copy_tral_fun <- function() {
  pryr::partial(
    copy_true_alignment
  )
}
