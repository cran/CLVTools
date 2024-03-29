#' @title Customer Lifetime Value Tools
#' @description
#' CLVTools is a toolbox for various probabilistic customer attrition models
#' for non-contractual settings. It provides a framework, which is capable
#' of unifying different probabilistic customer attrition models. This package
#' provides tools to estimate the number of future transactions of individual
#' customers as well as the probability of customers being alive in future
#' periods. Further, the average spending by customers can be estimated.
#' Multiplying the future transactions conditional on being alive and the
#' predicted individual spending per transaction results in an individual CLV value.
#'
#' The implemented models require transactional data from non-contractual
#' businesses (i.e. customers' purchase history).
#'
#' @seealso
#' Development for CLVTools can be followed via the GitHub repository
#' at \url{https://github.com/bachmannpatrick/CLVTools}.
#'
#' @examples
#'
#' \donttest{
#'
#' data("cdnow")
#'
#' # Create a CLV data object, split data in estimation and holdout sample
#' clv.data.cdnow <- clvdata(data.transactions = cdnow, date.format = "ymd",
#'                           time.unit = "week", estimation.split = 39, name.id = "Id")
#'
#' # summary of data
#' summary(clv.data.cdnow)
#'
#' # Fit a PNBD model without covariates on the first 39 periods
#' pnbd.cdnow <- pnbd(clv.data.cdnow,
#'                    start.params.model = c(r=0.5, alpha=8, s=0.5, beta=10))
#' # inspect fit
#' summary(pnbd.cdnow)
#'
#' # Predict 10 periods (weeks) ahead from estimation end
#' #   and compare to actuals in this period
#' pred.out <- predict(pnbd.cdnow, prediction.end = 10)
#'
#' # Plot the fitted model to the actual repeat transactions
#' plot(pnbd.cdnow)
#'
#' }
#'
#' @docType package
#' @useDynLib CLVTools, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @import data.table
#' @importFrom methods setClass
"_PACKAGE"


