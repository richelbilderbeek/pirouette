#' Internal function to create a \code{tree_and_model_errors}
#' from the files in one or more folders
#' @inheritParams default_params_doc
#' @return a \code{tree_and_model_errors},
#'   as can be checked by \link{check_tree_and_model_errors}
#' @author Richèl J.C. Bilderbeek
#' @export
create_tree_and_model_errors_from_folders <- function(# nolint indeed a long function name
  folder_names
) {
  tibbles <- list()
  for (i in seq_along(folder_names)) {
    tibbles[[i]] <- pirouette::create_tree_and_model_errors_from_folder(
      folder_names[i]
    )
  }
  dplyr::bind_rows(tibbles)
}
