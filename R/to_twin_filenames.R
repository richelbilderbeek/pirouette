#' Convert multiple filenames to their twin equivalent
#' @param filenames the paths to files, may be relative or absolute paths
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' filenames <- c("a.csv", "b.xml")
#' created <- to_twin_filenames(filenames)
#' expected <- c("a_twin.csv", "b_twin.xml")
#' expect_equal(expected, created)
#' @export
to_twin_filenames <- function(filenames) {
  for (i in seq_along(filenames)) {
    filenames[i] <- to_twin_filename(filenames[i]) # nolint pirouette function
  }
  filenames
}
