#' Update all babette dependencies
#' @noRd
update_pirouette <- function() {
  devtools::install_github("richelbilderbeek/babette", quiet = TRUE, dependencies = TRUE, upgrade = "always")
  devtools::install_github("richelbilderbeek/mcbette", quiet = TRUE, dependencies = TRUE, upgrade = "always")
  babette::update_babette()
}
