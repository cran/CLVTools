% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_interface_gg.R
\name{gg}
\alias{gg}
\alias{gg,clv.data-method}
\title{Gamma/Gamma Spending model}
\usage{
\S4method{gg}{clv.data}(
  clv.data,
  start.params.model = c(),
  remove.first.transaction = TRUE,
  optimx.args = list(),
  verbose = TRUE,
  ...
)
}
\arguments{
\item{clv.data}{The data object on which the model is fitted.}

\item{start.params.model}{Named start parameters containing the optimization start parameters for the model without covariates.}

\item{remove.first.transaction}{Whether customer's first transaction are removed. If \code{TRUE} all zero-repeaters are excluded from model fitting.}

\item{optimx.args}{Additional arguments to control the optimization which are forwarded to \code{\link[optimx:optimx]{optimx::optimx}}.
If multiple optimization methods are specified, only the result of the last method is further processed.}

\item{verbose}{Show details about the running of the function.}

\item{...}{Ignored}
}
\value{
An object of class \linkS4class{clv.gg} is returned.

The function \code{\link[CLVTools:summary.clv.fitted]{summary}} can be used to obtain and print a summary of the results.
The generic accessor functions \code{coefficients}, \code{\link[CLVTools:vcov.clv.fitted]{vcov}}, \code{\link[CLVTools:fitted.clv.fitted]{fitted}},
\code{logLik}, \code{AIC}, \code{BIC}, and \code{nobs} are available.
}
\description{
Fits the Gamma-Gamma model on a given object of class \code{clv.data} to predict customers' mean
spending per transaction.
}
\details{
Model parameters for the G/G model are \code{p, q, and gamma}. \cr
\code{p}: shape parameter of the Gamma distribution of the spending process. \cr
\code{q}: shape parameter of the Gamma distribution to account for customer heterogeneity. \cr
\code{gamma}: scale parameter of the Gamma distribution to account for customer heterogeneity.\cr
If no start parameters are given, p=0.5, q=15, gamma=2 is used for all model parameters. All parameters are required
to be > 0.

The Gamma-Gamma model cannot be estimated for data that contains negative prices.
Customers with a mean spending of zero or a transaction count of zero are ignored during model fitting.

\subsection{The G/G model}{
The G/G model allows to predict a value for future customer transactions. Usually, the G/G model is used
in combination with a probabilistic model predicting customer transaction such as the Pareto/NBD or the BG/NBD model.
}
}
\examples{
\donttest{
data("apparelTrans")
clv.data.apparel <- clvdata(apparelTrans, date.format = "ymd",
                            time.unit = "w", estimation.split = 40)

# Fit the gg model
gg(clv.data.apparel)

# Give initial guesses for the model parameters
gg(clv.data.apparel,
     start.params.model = c(p=0.5, q=15, gamma=2))


# pass additional parameters to the optimizer (optimx)
#    Use Nelder-Mead as optimization method and print
#    detailed information about the optimization process
apparel.gg <- gg(clv.data.apparel,
                     optimx.args = list(method="Nelder-Mead",
                                        control=list(trace=6)))

# estimated coefs
coef(apparel.gg)

# summary of the fitted model
summary(apparel.gg)

# Plot model vs empirical distribution
plot(apparel.gg)

# predict mean spending and compare against
#    actuals in the holdout period
predict(apparel.gg)
}
}
\references{
Colombo R, Jiang W (1999). \dQuote{A stochastic RFM model.}
Journal of Interactive Marketing, 13(3), 2-12.

Fader PS, Hardie BG, Lee K (2005). \dQuote{RFM and CLV: Using Iso-Value Curves for
Customer Base Analysis.} Journal of Marketing Research, 42(4), 415-430.

Fader PS, Hardie BG (2013). \dQuote{The Gamma-Gamma Model of Monetary Value.}
URL \url{http://www.brucehardie.com/notes/025/gamma_gamma.pdf}.
}
\seealso{
\code{\link[CLVTools:clvdata]{clvdata}} to create a clv data object.

\code{\link[CLVTools:plot.clv.data]{plot}} to plot diagnostics of the transaction data, incl. of spending.

\code{\link[CLVTools:predict.clv.fitted.spending]{predict}} to predict expected mean spending for every customer.

\code{\link[CLVTools:plot.clv.fitted.spending]{plot}} to plot the density of customer's mean transaction value compared to the model's prediction.
}
