% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_twal_with_same_n_mutation.R
\name{sim_twal_with_same_n_mutation}
\alias{sim_twal_with_same_n_mutation}
\title{Simulate a twin alignment using a standard site model}
\usage{
sim_twal_with_same_n_mutation(
  twin_phylogeny,
  true_alignment,
  root_sequence,
  mutation_rate = 1,
  site_model = beautier::create_jc69_site_model(),
  max_n_tries = 1000,
  verbose = FALSE
)
}
\arguments{
\item{twin_phylogeny}{a phylogeny of class \link[ape]{phylo}}

\item{true_alignment}{a DNA alignment, of class \link[ape]{DNAbin}}

\item{root_sequence}{the DNA sequence at the root of the phylogeny.
By default, this will consist out of an equal amount of each letter
Use \link{check_root_sequence} to check if a root sequence is valid.}

\item{mutation_rate}{the mutation rate per base pair per time unit.
Use \link{check_mutation_rate} to check if a mutation rate is valid.}

\item{site_model}{a nucleotide substitution model,
which can be:
\itemize{
  \item{
    A standard nucloetide substitution model,
    as created by \link[beautier]{create_site_model}
  }
  \item{
    \code{lns}: a linked node-substitution model
  }
  \item{
    \code{uns}: an unlinked node-substitution model
  }
}}

\item{max_n_tries}{maximum number of tries before giving up}

\item{verbose}{if TRUE, show more output}
}
\value{
an alignment
}
\description{
This is an adapter function
(see \url{https://en.wikipedia.org/wiki/Adapter_pattern}),
with the purpose of passing \link{check_sim_twal_fun},
by being a function with the function arguments \code{twin_phylogeny}
and \code{true_alignment}.
}
\examples{

# This adapter function must be a sim_twin_alignment function
check_sim_twal_fun(
  sim_twal_with_std_nsm
)

# Simulate a twin DNA alignment

alignment <- sim_twal_with_std_nsm(
  twin_phylogeny = ape::read.tree(text = "((A:1, B:1):2, C:3);"),
  root_sequence = "aaaa",
  mutation_rate = 0.1
)
check_alignment(alignment)
}
\seealso{
Use \link{sim_twin_alignment} to use this function to
create a twin alignment
}
\author{
Richèl J.C. Bilderbeek
}
