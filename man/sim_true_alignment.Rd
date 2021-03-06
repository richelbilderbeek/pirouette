% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_true_alignment.R
\name{sim_true_alignment}
\alias{sim_true_alignment}
\title{Simulate the true alignment from the true phylogeny}
\usage{
sim_true_alignment(
  true_phylogeny,
  alignment_params = pirouette::create_alignment_params(),
  verbose = FALSE
)
}
\arguments{
\item{true_phylogeny}{the true phylogeny; the actual evolutionary
history of the species, of class \link[ape]{phylo}}

\item{alignment_params}{parameters to simulate an alignment,
as can be created by \link{create_alignment_params}}

\item{verbose}{if TRUE, show more output}
}
\value{
an alignment of type \link[ape]{DNAbin}
}
\description{
Simulate the true alignment from the true phylogeny
}
\examples{

# Create the phylogeny to simulate the alignment on
n_taxa <- 5
true_phylogeny <- ape::rcoal(n_taxa)

root_sequence <- "aaaacgt"

# Use default settings to create the alignment
alignment_params <- create_alignment_params(
  sim_tral_fun =
    get_sim_tral_with_std_nsm_fun(
      mutation_rate = 1.0
  ),
  root_sequence = root_sequence
)

# Simulate the alignment
alignment <- sim_true_alignment(
  true_phylogeny = true_phylogeny,
  alignment_params = alignment_params,
)
check_alignment(alignment)
}
\seealso{
Use \link{create_tral_file} to save
the simulated alignment directly to a file
}
\author{
Richèl J.C. Bilderbeek
}
