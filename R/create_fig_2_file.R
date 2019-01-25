#' Create figure 2 file
#' @inheritParams default_params_doc
#' @return the name of the file the plot is saved to
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_fig_2_file <- function(
  project_folder_name = getwd()
) {
  # create figure
  fig_2 <- create_fig_2(project_folder_name) # nolint internal function

  # save output
  results_folder <- file.path(
    project_folder_name,
    results
  )
  # No warning if folder already exists
  dir.create(results_folder, showWarnings = FALSE)
  fig_2_filename <- file.path(
    results_folder,
    "figure_2.png"
  )
  ggplot2::ggsave(
    filename = fig_2_filename,
    plot = fig_2
  )
  fig_2_filename
}
