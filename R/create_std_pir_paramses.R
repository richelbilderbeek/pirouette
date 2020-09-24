#' Create a number of standard \code{pir_params}
#' @inheritParams default_params_doc
#' @param n number of \code{pir_params}
#' @return a \link{list} or \code{pir_params}, dubbed a \code{pir_paramses}.
#'   Use \link{check_pir_paramses} to check this list for validity.
#' @examples
#' pir_paramses <- create_std_pir_paramses(n = 2)
#' check_pir_paramses(pir_paramses)
#' @export
create_std_pir_paramses <- function(
  n,
  folder_name = rappdirs::user_cache_dir(),
  rng_seed = 314,
  crown_age = 10,
  sequence_length = 1000,
  mutation_rate = 1.0 / crown_age,
  os = rappdirs::app_dir()$os
) {
  pir_paramses <- list()
  for (i in seq_len(n)) {
    pir_paramses[[i]] <- create_std_pir_params(
      folder_name = folder_name,
      rng_seed = rng_seed,
      crown_age = crown_age,
      sequence_length = sequence_length,
      mutation_rate = mutation_rate,
      os = os
    )
  }

  # Renum seeds
  pir_paramses <- renum_rng_seeds(
    pir_paramses = pir_paramses,
    rng_seeds = seq(314, 314 - 1 + n)
  )

  # Rename files
  for (i in seq_along(pir_paramses)) {
    rng_seed <- pir_paramses[[i]]$alignment_params$rng_seed
    pir_paramses[[i]] <- pir_rename_to_std(
      pir_params = pir_paramses[[i]],
      folder_name = file.path(folder_name, rng_seed)
    )
  }

  pir_paramses
}
