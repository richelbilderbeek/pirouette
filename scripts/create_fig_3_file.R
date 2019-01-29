#' Create figure 3 file
#' @inheritParams default_params_doc
#' @return the name of the file the plot is saved to
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_fig_3_file <- function(
  project_folder_name = getwd()
) {
  # create figure
  fig_3 <- create_fig_3() # nolint internal function

  # save output
  results_folder <- file.path(
    project_folder_name,
    "results"
  )
  # No warning if folder already exists
  dir.create(results_folder, showWarnings = FALSE)
  fig_3_filename <- file.path(
    results_folder,
    "figure_3.png"
  )
  ggplot2::ggsave(
    filename = fig_3_filename,
    plot = fig_3
  )
  fig_3_filename
}
