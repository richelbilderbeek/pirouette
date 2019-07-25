#' Create all tables to checks \link{pirouette} pipeline
#' @inheritParams default_params_doc
#' @param folder folder where the files are stored in.
#'   By default, this is a temporary folder
#' @return the names of all files created
#' @author Richèl J.C. Bilderbeek
#' @export
pir_to_tables <- function(
  pir_params,
  folder = tempdir()
) {
  # The names of the files created
  filenames <- c()

  first_experiment <- pir_params$experiments[[1]]
  last_experiment <- pir_params$experiments[[length(pir_params$experiments)]]
  testit::assert(first_experiment$inference_model$mcmc$store_every != -1)
  testit::assert(last_experiment$inference_model$mcmc$store_every != -1)

  ##############################################################################
  # Evidence
  ##############################################################################
  # Very custom layout function
  tidy_df <- function(df) {
    df$site_model_name <- plyr::revalue(
      df$site_model_name, c("JC69" = "JC", "TN93" = "TN"),
      warn_missing = FALSE
    )
    df$clock_model_name <- plyr::revalue(
      df$clock_model_name,
      c("strict" = "Strict", "relaxed_log_normal" = "RLN"),
      warn_missing = FALSE
    )
    df$tree_prior_name <- plyr::revalue(
      df$tree_prior_name,
      c(
        "yule" = "Yule",
        "birth_death" = "BD",
        "coalescent_bayesian_skyline" = "CBS",
        "coalescent_constant_population" = "CCP",
        "coalescent_exp_population" = "CEP"
      ),
      warn_missing = FALSE
    )
    names(df) <- c(
      "Site model", "Clock model", "Tree prior", "log(evidence)", "Weight"
    )
    df
  }

  if (last_experiment$inference_conditions$model_type == "candidate") {
    ################
    # Evidence, true
    ################
    df <- tidy_df(
      utils::read.csv(pir_params$evidence_filename)[, c(-1, -6)]
    )

    filename <- file.path(folder, "evidence_true.latex")
    filenames <- c(filenames, filename)
    sink(filename)
    xtable::print.xtable(
      xtable::xtable(
        df,
        caption = "Evidences for the true phylogeny", digits = 3
      ),
      include.rownames = FALSE
    )
    sink()

    if (!beautier::is_one_na(pir_params$twinning_params)) {
      ################
      # Evidence, twin
      ################
      df <- tidy_df(
        utils::read.csv(
          pir_params$twinning_params$twin_evidence_filename
        )[, c(-1, -6)]
      )

      filename <- file.path(folder, "evidence_twin.latex")
      filenames <- c(filenames, filename)
      sink(filename)
      xtable::print.xtable(
        xtable::xtable(
          df,
          caption = "Evidences for twin phylogeny", digits = 3
        ),
        include.rownames = FALSE
      )
      sink()
    }
  }

  ##############################################################################
  # ESS
  ##############################################################################
  if (first_experiment$inference_conditions$model_type == "generative") {
    #######################
    # Generative, true tree
    #######################
    esses_gen <- tracerer::calc_esses(
      traces = tracerer::parse_beast_log(
        first_experiment$beast2_options$output_log_filename
      ),
      sample_interval = first_experiment$inference_model$mcmc$store_every
    )
    df_esses_gen <- data.frame(
      parameter = colnames(esses_gen),
      ESS = as.character(esses_gen)
    )
    filename <- file.path(folder, "esses_gen.latex")
    filenames <- c(filenames, filename)
    sink(filename)
    xtable::print.xtable(
      xtable::xtable(
        df_esses_gen,
        caption = paste0("ESSes for generative model"),
        digits = 0
      ),
      include.rownames = FALSE
    )
    sink()
    #######################
    # Generative, twin tree
    #######################
    if (!beautier::is_one_na(pir_params$twinning_params)) {
      esses_twin_gen <- tracerer::calc_esses(
        traces = tracerer::parse_beast_log(to_twin_filename(
          first_experiment$beast2_options$output_log_filename)
        ),
        sample_interval = first_experiment$inference_model$mcmc$store_every
      )
      df_esses_twin_gen <- data.frame(
        parameter = colnames(esses_twin_gen),
        ESS = as.character(esses_twin_gen)
      )
      filename <- file.path(folder, "esses_twin_gen.latex")
      filenames <- c(filenames, filename)
      sink(filename)
      xtable::print.xtable(
        xtable::xtable(
          df_esses_twin_gen,
          caption = paste0("ESSes for generative model, twin tree"),
          digits = 0
        ),
        include.rownames = FALSE
      )
      sink()
    }
  }
  if (last_experiment$inference_conditions$model_type == "candidate") {
    #######################
    # Candidate, true tree
    #######################
    esses_best <- tracerer::calc_esses(
      traces = tracerer::parse_beast_log(
        last_experiment$beast2_options$output_log_filename
      ),
      sample_interval = last_experiment$inference_model$mcmc$store_every
    )
    df_esses_best <- data.frame(
      parameter = colnames(esses_best),
      ESS = as.character(esses_best)
    )
    filename <- file.path(folder, "esses_best.latex")
    filenames <- c(filenames, filename)
    sink(filename)
    xtable::print.xtable(
      xtable::xtable(
        df_esses_best,
        caption = paste0("ESSes for best candidate model"),
        digits = 0
      ),
      include.rownames = FALSE
    )
    sink()

    #######################
    # Candidate, twin tree
    #######################
    if (!beautier::is_one_na(pir_params$twinning_params)) {
      esses_twin_best <- tracerer::calc_esses(
        traces = tracerer::parse_beast_log(to_twin_filename(
          last_experiment$beast2_options$output_log_filename)
        ),
        sample_interval = last_experiment$inference_model$mcmc$store_every
      )
      df_esses_twin_best <- data.frame(
        parameter = colnames(esses_twin_best),
        ESS = as.character(esses_twin_best)
      )
      filename <- file.path(folder, "esses_twin_best.latex")
      filenames <- c(filenames, filename)
      sink(filename)
      xtable::print.xtable(
        xtable::xtable(
          df_esses_twin_best,
          caption = paste0("ESSes for best candidate model, twin tree"),
          digits = 0
        ),
        include.rownames = FALSE
      )
      sink()
    }
  }
  filenames
}