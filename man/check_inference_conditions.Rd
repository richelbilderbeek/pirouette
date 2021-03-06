% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_inference_conditions.R
\name{check_inference_conditions}
\alias{check_inference_conditions}
\title{Checks if inference conditions are valid
as created by \link{create_inference_conditions}.
Will \link{stop} if not.}
\usage{
check_inference_conditions(inference_conditions)
}
\arguments{
\item{inference_conditions}{conditions under which the inference model
is used in the inference}
}
\value{
nothing. Will \link{stop} if not
}
\description{
Checks if inference conditions are valid
as created by \link{create_inference_conditions}.
Will \link{stop} if not.
}
\examples{
check_inference_conditions(create_inference_conditions())
}
\author{
Giovanni Laudanno, Richèl J.C. Bilderbeek
}
