#' Create a \code{pir_out} from a folder
#'
#' @inheritParams default_params_doc
#' @return a \code{pir_out}, as can be checked by \link{check_pir_out}
#' @author Richèl J.C. Bilderbeek
#' @export
create_pir_outs_from_folders <- function(
  folder_names
) {
  pir_outs <- list()

  for (i in seq_along(folder_names)) {
    pir_outs[[i]] <- create_pir_out_from_folder(folder_names[i])
  }

  pir_outs
}
