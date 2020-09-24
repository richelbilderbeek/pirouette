#' Convert multiple filenames to their twin equivalent
#' @param filenames the paths to files, may be relative or absolute paths
#' @author Richèl J.C. Bilderbeek
#' @examples
#' filenames <- c("a.csv", "b.xml")
#' # c("a_twin.csv", "b_twin.xml")
#' to_twin_filenames(filenames)
#' @export
to_twin_filenames <- function(filenames) {
  for (i in seq_along(filenames)) {
    filenames[i] <- pirouette::to_twin_filename(filenames[i])
  }
  filenames
}
