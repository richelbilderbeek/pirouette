#' Create figure 4 file
#' @inheritParams default_params_doc
#' @return the name of the file the plot is saved to
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_fig_4_file <- function(
  project_folder_name = getwd()
) {
  # create figure
  fig_4 <- create_fig_4(project_folder_name) # nolint internal function

  # save output
  results_folder <- file.path(
    project_folder_name,
    results
  )
  # No warning if folder already exists
  dir.create(results_folder, showWarnings = FALSE)
  fig_4_filename <- file.path(
    results_folder,
    "figure_4.png"
  )
  ggplot2::ggsave(
    filename = fig_4_filename,
    plot = fig_4
  )
  fig_4_filename
}
