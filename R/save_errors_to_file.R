#' Save errors estimated with pir_run to files
#' @param df output dataframe from pir_run
#' @inheritParams default_params_doc
#' @return nothing
#' @author Giovanni Laudanno
#' @noRd
save_errors_to_file <- function(
  df,
  pir_params
) {
  for (r in 1:nrow(df)) {
    dir <- dirname(pir_params$error_measure_params$errors_filename)
    base <- basename(pir_params$error_measure_params$errors_filename)
    row <- df[r, ]
    setting <- row[mapply(row, FUN = function(x) !is.numeric(x)) & !is.na(row)]
    filename <- file.path(dir, paste(c(setting, base), collapse = "-"))
    utils::write.csv(
      x = df[, grepl("error", colnames(df))],
      file = filename
    )
    testit::assert(file.exists(filename))
  }
  invisible()
}
