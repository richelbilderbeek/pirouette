#' Renumber the RNG seeds
#' @inheritParams default_params_doc
#' @export
renum_rng_seeds <- function(
  pir_paramses,
  rng_seeds
) {
  stopifnot(length(pir_paramses) == length(rng_seeds))
  for (i in seq_along(pir_paramses)) {
    # This pir_paramses' RNG seed
    rng_seed <- rng_seeds[i]

    pir_paramses[[i]]$alignment_params$rng_seed <- rng_seed

    # Experiments
    for (j in seq_along(pir_paramses[[i]]$experiments)) {
      pir_paramses[[i]]$experiments[[j]]$beast2_options$rng_seed <- rng_seed
    }

    # Twin
    if (pirouette::has_twinning(pir_paramses[[i]])) {
      pir_paramses[[i]]$twinning_params$rng_seed_twin_tree <- rng_seed
      pir_paramses[[i]]$twinning_params$rng_seed_twin_alignment <- rng_seed
    }
  }
  pir_paramses
}

