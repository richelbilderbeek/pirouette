#' Plot the error 'BEAST2' makes from a known phylogeny
#' from tidy data
#' @inheritParams default_params_doc
#' @return a \code{ggplot2} plot
#' @author Richèl J.C. Bilderbeek
#' @export
create_long_pir_out_from_folders <- function(# nolint indeed a long function name
  folder_names
) {
  tibbles <- list()
  for (i in seq_along(folder_names)) {
    tibbles[[i]] <- create_long_pir_out_from_folder(folder_names[i])
  }
  dplyr::bind_rows(tibbles)
}
