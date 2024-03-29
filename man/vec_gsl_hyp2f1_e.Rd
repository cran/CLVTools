% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{vec_gsl_hyp2f1_e}
\alias{vec_gsl_hyp2f1_e}
\title{GSL Hypergeometric 2F1 for equal length vectors}
\usage{
vec_gsl_hyp2f1_e(vA, vB, vC, vZ)
}
\arguments{
\item{vA}{Vector of values for parameter a}

\item{vB}{Vector of values for parameter b}

\item{vC}{Vector of values for parameter c}

\item{vZ}{Vector of values for parameter z}
}
\value{
List with vector of values and vector of gsl status codes
}
\description{
Calculate the hypergeometric 2f1 using the GSL library (gsl_sf_hyperg_2F1_e)
}
\keyword{internal}
