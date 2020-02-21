#' Delete the BEAST2 state files, if present.
#' @inheritParams default_params_doc
#' @export
delete_beast2_state_files <- function(
  beast2_optionses,
  verbose = FALSE
) {
  for (beast2_options in beast2_optionses) {
    if (file.exists(beast2_options$output_state_filename)) {
      if (isTRUE(verbose)) {
        message(
          paste0("Deleting file '",
            beast2_options$output_state_filename, "'"
          )
        )
      }
      file.remove(beast2_options$output_state_filename)
    }
  }
}
