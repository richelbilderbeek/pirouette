#' Save all output from \link{pir_run}
#' @inheritParams default_params_doc
#' @return nothing
#' @export
pir_save <- function(
  phylogeny,
  pir_params,
  pir_out,
  folder_name
) {
  # Create a folder, do not warn if it already exists
  dir.create(folder_name, recursive = TRUE, showWarnings = FALSE)
  ape::write.tree(
    phylogeny,
    file = file.path(folder_name, "phylogeny.newick")
  )

  utils::write.csv(
    x = pir_out,
    file = file.path(folder_name, "errors.csv"),
    row.names = FALSE
  )

  pir_plot(pir_out)
  ggplot2::ggsave(file.path(folder_name, "errors.png"))

  pir_to_tables(
    pir_params = pir_params,
    folder = folder_name
  )
  pir_to_pics(
    phylogeny = phylogeny,
    pir_params = pir_params,
    folder = folder_name
  )
  invisible(phylogeny)
}
