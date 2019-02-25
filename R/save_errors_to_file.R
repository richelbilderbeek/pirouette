#' Save errors estimated with pir_run to files
#' @param df output dataframe from pir_run
#' @inheritParams default_params_doc
#' @return nothing
#' @author Giovanni Laudanno
#' @noRd
save_errors_to_file <- function(
  df,
  pir_params
) {2
  check_pir_params(pir_params) # nolint pirouette function
  filename <- pir_params$error_measure_params$errors_filename
  utils::write.csv(
    x = df,
    file = filename
  )
  invisible()
}
