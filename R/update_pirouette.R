#' Update all babette dependencies
#' @inheritParams remotes::install_github
#' @examples
#' library(testthat)
#'
#' if (is_on_travis()) {
#'
#'   # Updates the pirouette dependencies without asking
#'   update_pirouette(upgrade = "always")
#'
#'   # Updating again should produce no output,
#'   # as there is nothing to upgrade left
#'   expect_silent(update_pirouette(upgrade = "always"))
#' }
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @export
update_pirouette <- function(upgrade = "default") {
  remotes::install_github(
    "richelbilderbeek/babette",
    quiet = TRUE, dependencies = TRUE, upgrade = upgrade
  )
  remotes::install_github(
    "richelbilderbeek/mcbette",
    quiet = TRUE, dependencies = TRUE, upgrade = upgrade
  )
  babette::update_babette(upgrade = upgrade)
}
