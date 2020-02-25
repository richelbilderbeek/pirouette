#' Replicate pir_params assigning new names to each file
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno
#' @return a list of replicated pir_params.
#' @export
replicate_pir_params <- function(
  pir_params,
  n_replicates
) {
  # checks
  pirouette::check_pir_params(pir_params)
  if ((n_replicates %% 1) != 0) {
    stop("'n_replicates' must be an integer")
  }
  if (n_replicates <= 0) {
    stop("'n_replicates' must be positive")
  }

  pir_paramses <- vector("list", n_replicates)
  for (i in seq_along(pir_paramses)) {
    set.seed(i)
    pir_paramses[[i]] <- pir_rename(
      pir_params = pir_params,
      rename_fun = beautier::get_remove_dir_fun()
    )
    pirouette::check_pir_params(pir_paramses[[i]])
  }
  pir_paramses
}
