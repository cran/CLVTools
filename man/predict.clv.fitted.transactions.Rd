% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_interface_predict_clvfittedtransactions.R
\name{predict.clv.fitted.transactions}
\alias{predict.clv.fitted.transactions}
\alias{predict}
\alias{predict,clv.fitted.transactions-method}
\title{Predict CLV from a fitted transaction model}
\usage{
\method{predict}{clv.fitted.transactions}(
  object,
  newdata = NULL,
  prediction.end = NULL,
  predict.spending = gg,
  continuous.discount.factor = log(1 + 0.1),
  uncertainty = c("none", "boots"),
  level = 0.9,
  num.boots = 100,
  verbose = TRUE,
  ...
)

\S4method{predict}{clv.fitted.transactions}(
  object,
  newdata = NULL,
  prediction.end = NULL,
  predict.spending = gg,
  continuous.discount.factor = log(1 + 0.1),
  uncertainty = c("none", "boots"),
  level = 0.9,
  num.boots = 100,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{object}{A fitted clv transaction model for which prediction is desired.}

\item{newdata}{A clv data object or data for the new customer prediction (see \link{newcustomer}) for which predictions should
be made with the fitted model. If none or NULL is given, predictions are made for the data on which the model was fit.}

\item{prediction.end}{{} Until what point in time to predict. This can be the number of periods (numeric) or a form of date/time object. See details.}

\item{predict.spending}{Whether and how to predict spending and based on it also CLV, if possible. See details.}

\item{continuous.discount.factor}{continuous discount factor to use to calculate \code{DERT/DECT}. Defaults to a 10\% continuous annual rate. See details.}

\item{uncertainty}{Method to produce confidence intervals of the predictions (parameter uncertainty). Either "none" (default) or "boots".}

\item{level}{Required confidence level, if \code{uncertainty="boots"}.}

\item{num.boots}{Number of bootstrap repetitions, if \code{uncertainty="boots"}. A low number may not produce intervals for all customers if they are not sampled.}

\item{verbose}{Show details about the running of the function.}

\item{...}{Ignored}
}
\value{
An object of class \code{data.table} with columns:
\item{Id}{The respective customer identifier}
\item{period.first}{First timepoint of prediction period}
\item{period.last}{Last timepoint of prediction period}
\item{period.length}{Number of time units covered by the period indicated by \code{period.first} and \code{period.last} (including both ends).}
\item{PAlive}{Probability to be alive at the end of the estimation period}
\item{CET}{The Conditional Expected Transactions: The number of transactions expected until prediction.end.}
\item{DERT or DECT}{Discounted Expected Residual Transactions or Discounted Expected Conditional Transactions for dynamic covariates models}
\item{actual.x}{Actual number of transactions until prediction.end. Only if there is a holdout period and the prediction ends in it, otherwise not reported.}
\item{actual.total.spending}{Actual total spending until prediction.end. Only if there is a holdout period and the prediction ends in it, otherwise not reported.}
\item{predicted.mean.spending}{The mean spending per transactions as predicted by the spending model.}
\item{predicted.total.spending}{The predicted total spending until prediction.end (\code{CET*predicted.mean.spending}).}
\item{predicted.CLV}{Customer Lifetime Value based on \code{DERT/DECT} and \code{predicted.mean.spending}.}

If predicting for new customers (using \code{newcustomer()}), a numeric scalar indicating the expected
number of transactions is returned instead.
}
\description{
Probabilistic customer attrition models predict in general three expected characteristics for every customer:
\itemize{
\item "conditional expected transactions" (\code{CET}), which is the number of transactions to expect from a customer
during the prediction period,
\item "probability of a customer being alive" (\code{PAlive}) at the end of the estimation period and
\item "discounted expected residual transactions" (\code{DERT}) for every customer, which is the total number of
transactions for the residual lifetime of a customer discounted to the end of the estimation period.
In the case of time-varying covariates, instead of \code{DERT}, "discounted expected conditional transactions" (\code{DECT})
is predicted. \code{DECT} does only cover a finite time horizon in contrast to \code{DERT}.
For \code{continuous.discount.factor=0}, \code{DECT} corresponds to \code{CET}.
}

In order to derive a monetary value such as CLV, customer spending has to be considered.
If the \code{clv.data} object contains spending information, customer spending can be predicted using a Gamma/Gamma spending model for
parameter \code{predict.spending} and the predicted CLV is be calculated (if the transaction model supports \code{DERT/DECT}).
In this case, the prediction additionally contains the following two columns:
\itemize{
\item "predicted.mean.spending", the mean spending per transactions as predicted by the spending model.
\item "CLV", the customer lifetime value. CLV is the product of DERT/DECT and predicted spending.
}

Uncertainty estimates are available for all predicted quantities using bootstrapping.

\subsection{New customer prediction}{
The fitted model can also be used to predict the number of transactions a single, average
newly alive customer is expected to make at the moment of the first transaction ("coming alive").
For covariate models, the prediction is for an average customer with the given covariates.

The individual-level unconditional expectation that is also used for the
\link[CLVTools:plot.clv.fitted.transactions]{tracking plot} is used to obtain this prediction.
For models without covariates, the prediction hence is the same for all customers
and independent of when a customer comes alive.
For models with covariates, the prediction is the same for all customers
with the same covariates.

The data on which the model was fit and which is stored in it is NOT used for this prediction.
See examples and \link{newcustomer} for more details.
}
}
\details{
\code{predict.spending} indicates whether to predict customers' spending and if so, the spending model to use.
Accepted inputs are either a logical (\code{TRUE/FALSE}), a method to fit a spending model (i.e. \code{\link{gg}}), or
an already fitted spending model. If provided \code{TRUE}, a Gamma-Gamma model is fit with default options. If argument
\code{newdata} is provided, the spending model is fit on \code{newdata}. Predicting spending is only possible if
the transaction data contains spending information. See examples for illustrations of valid inputs.

The \code{newdata} argument has to be a clv data object of the exact same class as the data object
on which the model was fit. In case the model was fit with covariates, \code{newdata} needs to contain identically
named covariate data.

The use case for \code{newdata} is mainly two-fold: First, to estimate model parameters only on a
sample of the data and then use the fitted model object to predict or plot for the full data set provided through \code{newdata}.
Second, for models with dynamic covariates, to provide a clv data object with longer covariates than contained in the data
on which the model was estimated what allows to predict or plot further. When providing \code{newdata}, some models
might require additional steps that can significantly increase runtime.

To predict for new customers, the output of \link{newcustomer} has to be given to \code{newdata}. See examples.

\code{prediction.end} indicates until when to predict or plot and can be given as either
a point in time (of class \code{Date}, \code{POSIXct}, or \code{character}) or the number of periods.
If \code{prediction.end} is of class character, the date/time format set when creating the data object is used for parsing.
If \code{prediction.end} is the number of periods, the end of the fitting period serves as the reference point
from which periods are counted. Only full periods may be specified.
If \code{prediction.end} is omitted or NULL, it defaults to the end of the holdout period if present and to the
end of the estimation period otherwise.

The first prediction period is defined to start right after the end of the estimation period.
If for example weekly time units are used and the estimation period ends on Sunday 2019-01-01, then the first day
of the first prediction period is Monday 2019-01-02. Each prediction period includes a total of 7 days and
the first prediction period therefore will end on, and include, Sunday 2019-01-08. Subsequent prediction periods
again start on Mondays and end on Sundays.
If \code{prediction.end} indicates a timepoint on which to end, this timepoint is included in the prediction period.

\code{continuous.discount.factor} is the continuous rate used to discount the expected residual
transactions (\code{DERT/DECT}). An annual rate of (100 x d)\% equals a continuous rate delta = ln(1+d).
To account for time units which are not annual, the continuous rate has to be further adjusted
to delta=ln(1+d)/k, where k are the number of time units in a year.
}
\section{Uncertainty Estimates}{

Bootstrapping is used to provide confidence intervals of all predicted metrics.
These provide an estimate of parameter uncertainty.
To create bootstrapped data, customer ids are sampled with replacement until reaching original
length and all transactions of the sampled customers are used to create a new \code{clv.data} object.
A new model is fit on the bootstrapped data with the same specification as \code{object}
(incl. start parameters and `optimx.args`) and it is then used to predict on this data.
All prediction parameters, incl \code{prediction.end} and \code{continuous.discount.factor}, are forwarded
to the prediction on the bootstrapped data.
Per customer, confidence intervals of each predicted metric are created using a "reversed quantile" approach.
See \link{clv.bootstrapped.apply} to create a custom bootstrapping procedure.
}

\examples{

\donttest{

data("apparelTrans")
# Fit pnbd standard model on data, WITH holdout
apparel.holdout <- clvdata(apparelTrans, time.unit="w",
                           estimation.split=37, date.format="ymd")
apparel.pnbd <- pnbd(apparel.holdout)

# Predict until the end of the holdout period
predict(apparel.pnbd)

# Predict until 10 periods (weeks in this case) after
#   the end of the 37 weeks fitting period
predict(apparel.pnbd, prediction.end = 10) # ends on 2010-11-28

# Predict until 31th Dec 2016 with the timepoint as a character
predict(apparel.pnbd, prediction.end = "2016-12-31")

# Predict until 31th Dec 2016 with the timepoint as a Date
predict(apparel.pnbd, prediction.end = lubridate::ymd("2016-12-31"))


# Predict future transactions but not spending and CLV
predict(apparel.pnbd, predict.spending = FALSE)

# Predict spending by fitting a Gamma-Gamma model
predict(apparel.pnbd, predict.spending = gg)

# Fit a spending model separately and use it to predict spending
apparel.gg <- gg(apparel.holdout, remove.first.transaction = FALSE)
predict(apparel.pnbd, predict.spending = apparel.gg)


# Fit pnbd standard model WITHOUT holdout
pnc <- pnbd(clvdata(apparelTrans, time.unit="w", date.format="ymd"))

# This fails, because without holdout, a prediction.end is required
\dontrun{
predict(pnc)
}

# But it works if providing a prediction.end
predict(pnc, prediction.end = 10) # ends on 2016-12-17


# Predict num transactions for a newly alive customer
# in the next 3.45 weeks
# See ?newcustomer() for more examples
predict(apparel.pnbd, newdata = newcustomer(num.periods=3.45))

}



}
\seealso{
models to predict transactions: \link{pnbd}, \link{bgnbd}, \link{ggomnbd}.

models to predict spending: \link{gg}.

\code{\link[CLVTools:predict.clv.fitted.spending]{predict}} for spending models

\link{clv.bootstrapped.apply} for bootstrapped model estimation

\code{\link{newcustomer}} to create data to predict for newly alive customers.
}
