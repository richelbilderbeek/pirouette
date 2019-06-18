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

  # The files to be created
  filenames <- c(
    file.path(folder, "evidence_true.latex"),
    file.path(folder, "evidence_twin.latex")
  )

  # Evidence, true
  df <- tidy_df(
    utils::read.csv(pir_params$evidence_filename)[, c(-1, -6)]
  )

  sink(filenames[1])
  xtable::print.xtable(
    xtable::xtable(
      df,
      caption = "Evidences for the true phylogeny", digits = 3
    ),
    include.rownames = FALSE
  )
  sink()

  # Evidence, twin
  df <- tidy_df(
    utils::read.csv(
      pir_params$twinning_params$twin_evidence_filename
    )[, c(-1, -6)]
  )

  sink(filenames[2])
  xtable::print.xtable(
    xtable::xtable(
      df,
      caption = "Evidences for twin phylogeny", digits = 3
    ),
    include.rownames = FALSE
  )
  sink()

  filenames
}
