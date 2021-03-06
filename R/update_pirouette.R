#' Update all babette dependencies
#' @inheritParams remotes::install_github
#' @examples
#' \dontrun{
#'   # Updates the pirouette dependencies without asking
#'   update_pirouette(upgrade = "always")
#'
#'   # Updating again should produce no output,
#'   # as there is nothing to upgrade left
#'   update_pirouette(upgrade = "always")
#' }
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @export
update_pirouette <- function(upgrade = "default") {
  remotes::install_github(
    "richelbilderbeek/babette",
    quiet = TRUE, dependencies = TRUE, upgrade = upgrade
  )
  remotes::install_github(
    "ropensci/mcbette",
    quiet = TRUE, dependencies = TRUE, upgrade = upgrade
  )
  babette::update_babette(upgrade = upgrade)
}
