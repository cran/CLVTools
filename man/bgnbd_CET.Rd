% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{bgnbd_CET}
\alias{bgnbd_CET}
\alias{bgnbd_nocov_CET}
\alias{bgnbd_staticcov_CET}
\title{BG/NBD: Conditional Expected Transactions}
\usage{
bgnbd_nocov_CET(r, alpha, a, b, dPeriods, vX, vT_x, vT_cal)

bgnbd_staticcov_CET(
  r,
  alpha,
  a,
  b,
  dPeriods,
  vX,
  vT_x,
  vT_cal,
  vCovParams_trans,
  vCovParams_life,
  mCov_trans,
  mCov_life
)
}
\arguments{
\item{r}{shape parameter of the Gamma distribution of the purchase process}

\item{alpha}{scale parameter of the Gamma distribution of the purchase process}

\item{a}{shape parameter of the Beta distribution of the lifetime process}

\item{b}{shape parameter of the Beta distribution of the lifetime process}

\item{dPeriods}{number of periods to predict}

\item{vX}{Frequency vector of length n counting the numbers of purchases.}

\item{vT_x}{Recency vector of length n.}

\item{vT_cal}{Vector of length n indicating the total number of periods of observation.}

\item{vCovParams_trans}{Vector of estimated parameters for the transaction covariates.}

\item{vCovParams_life}{Vector of estimated parameters for the lifetime covariates.}

\item{mCov_trans}{Matrix containing the covariates data affecting the transaction process. One column for each covariate.}

\item{mCov_life}{Matrix containing the covariates data affecting the lifetime process. One column for each covariate.}
}
\value{
Returns a vector containing the conditional expected transactions for the existing
customers in the BG/NBD model.
}
\description{
Calculates the expected number of transactions in a given time period based
on a customer's past transaction behavior and the BG/NBD model parameters.

\describe{
\item{\code{bgnbd_nocov_CET}}{ Conditional Expected Transactions without covariates}
\item{\code{bgnbd_staticcov_CET}}{ Conditional Expected Transactions with static covariates}
}
}
\details{
\code{mCov_trans} is a matrix containing the covariates data of
the time-invariant covariates that affect the transaction process.
Each column represents a different covariate. For every column a gamma parameter
needs to added to \code{vCovParams_trans} at the respective position.

\code{mCov_life} is a matrix containing the covariates data of
the time-invariant covariates that affect the lifetime process.
Each column represents a different covariate. For every column a gamma parameter
needs to added to \code{vCovParams_life} at the respective position.
}
\references{
Fader PS, Hardie BGS, Lee KL (2005). \dQuote{\dQuote{Counting Your Customers} the Easy Way:
An Alternative to the Pareto/NBD Model} Marketing Science, 24(2), 275-284.

Fader PS, Hardie BGS (2013). \dQuote{Overcoming the BG/NBD Model's #NUM! Error Problem}
URL \url{http://brucehardie.com/notes/027/bgnbd_num_error.pdf}.

Fader PS, Hardie BGS (2007). \dQuote{Incorporating time-invariant covariates into the
Pareto/NBD and BG/NBD models.}
URL \url{http://www.brucehardie.com/notes/019/time_invariant_covariates.pdf}.

Fader PS, Hardie BGS, Lee KL (2007). \dQuote{Creating a Fit Histogram for the BG/NBD Model}
URL \url{https://www.brucehardie.com/notes/014/bgnbd_fit_histogram.pdf}
}
