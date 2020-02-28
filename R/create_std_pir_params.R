#' Create a standard \code{pir_params}, as used in the paper
#' @inheritParams default_params_doc
#' @export
create_std_pir_params <- function(
  folder_name = rappdirs::user_cache_dir(),
  rng_seed = 314,
  crown_age = 10,
  sequence_length = 1000,
  mutation_rate = 1.0 / crown_age,
  os = rappdirs::app_dir()$os
) {
  stopifnot(rng_seed >= 2)
  stopifnot(crown_age > 0.0)
  stopifnot(sequence_length >= 1)
  stopifnot(sequence_length >= 1)
  pirouette::check_mutation_rate(mutation_rate)
  beastier::check_os(os)

  alignment_params <- create_alignment_params(
    sim_tral_fun = get_sim_tral_with_std_nsm_fun(
      mutation_rate = mutation_rate
    ),
    root_sequence = create_blocked_dna(length = sequence_length)
  )

  # Create the experiments
  experiments <- NA
  evidence_filename <- NA
  twin_evidence_filename <- NA
  # Hand-pick a generating model
  # By default, this is JC69, strict, Yule
  generative_experiment <- create_gen_experiment()
  if (os != "win") {
    # Create the set of candidate birth-death experiments
    candidate_experiments <- create_all_bd_experiments(
      exclude_model = generative_experiment$inference_model
    )
    # Combine all experiments
    experiments <- c(list(generative_experiment), candidate_experiments)

    evidence_filename <- get_temp_evidence_filename()
    twin_evidence_filename <- get_temp_evidence_filename()
  } else {
    experiments <- list()
    experiments[[1]] <- generative_experiment
  }

  twinning_params <- create_twinning_params(
    sim_twin_tree_fun = get_sim_bd_twin_tree_fun(),
    sim_twal_fun = get_sim_twal_same_n_muts_fun(
      mutation_rate = mutation_rate,
      max_n_tries = 10000
    ),
    twin_evidence_filename = twin_evidence_filename
  )

  pir_params <- create_pir_params(
    alignment_params = alignment_params,
    experiments = experiments,
    twinning_params = twinning_params,
    evidence_filename = evidence_filename
  )


  # Rename
  pir_params <- pir_rename_to_std(
    pir_params = pir_params,
    folder_name = folder_name
  )

  # Seed
  pir_params <- renum_rng_seeds(
    pir_paramses = list(pir_params),
    rng_seeds = rng_seed
  )[[1]]

  pir_params
}
