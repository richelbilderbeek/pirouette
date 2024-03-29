% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/select_candidate_evidences.R
\name{select_candidate_evidences}
\alias{select_candidate_evidences}
\title{Select the evidences for candidate experiments}
\usage{
select_candidate_evidences(
  experiments = list(create_test_experiment()),
  marg_liks = create_test_marg_liks()
)
}
\arguments{
\item{experiments}{a list of one or more \link{pirouette} experiments,
as can be created by \link{create_experiment}. If more than one experiment
is provided and a "generative" experiment is part of them, the "generative"
one has to be the first in the list. See also:
\itemize{
  \item Use \link{check_experiments} to check the list of
    experiments for validity
  \item Use \link{create_all_experiments} to create experiments with
    all combinations of tree model, clock model and tree priors
  \item Use \link{create_all_bd_experiments} to create experiments
    with all combinations of tree model, clock model and tree priors,
    except for only using birth-death tree priors
  \item Use \link{create_all_coal_experiments} to create all experiments
    with all combinations of tree model, clock model and tree priors,
    except for only coalescent tree priors
  \item Use \link{shorten_experiments} to shorten the run time
    of the list of experiments
}}

\item{marg_liks}{a data frame with marginal likelihoods/evidences.
A test data frame can be created by \link{create_test_marg_liks}}
}
\value{
the evidences for the candidate experiments, as a numeric vector
}
\description{
Select the evidences for candidate experiments
}
\examples{
if (beautier::is_on_ci() && rappdirs::app_dir()$os != "win") {
  experiment_1 <- create_test_gen_experiment()
  experiment_2 <- create_test_cand_experiment()
  experiments <- list(experiment_1, experiment_2)

  # Experiments must have different inference models
  experiments[[1]]$inference_model$site_model <- 
    beautier::create_gtr_site_model()

  select_candidate_evidences(
    experiments = experiments,
    marg_liks = create_test_marg_liks()
  )
}
}
\author{
Richèl J.C. Bilderbeek
}
