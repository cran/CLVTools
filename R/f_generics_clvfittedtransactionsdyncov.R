# . clv.controlflow.plot.check.inputs ------------------------------------------------------------------------
setMethod("clv.controlflow.plot.check.inputs", signature(obj="clv.fitted.transactions.dynamic.cov"), function (obj, prediction.end, cumulative, plot, label.line, verbose) {
  period.until <- Cov.Date <- NULL

  # No nocov /staticcov checks (no need to call super method)
  err.msg <- c()

  # Check that dyncov covariate is long enough for prediction end
  #   Convert prediction.end already for this
  dt.expectation <- clv.time.expectation.periods(clv.time = obj@clv.data@clv.time, user.tp.end = prediction.end)

  # only need to check one cov data, guaranteed that both are of same length
  if(dt.expectation[, max(period.until)] > obj@clv.data@data.cov.trans[, max(Cov.Date)])
    err.msg <- c(err.msg, "The dynamic covariates in the fitted model are not long enough for the given prediction.end!")

  check_err_msg(err.msg)
})


# . clv.controlflow.check.newdata ------------------------------------------------------------------------
#' @importFrom methods callNextMethod
setMethod("clv.controlflow.check.newdata", signature(clv.fitted="clv.fitted.transactions.dynamic.cov"), definition = function(clv.fitted, user.newdata, prediction.end, ...){
  # Do static cov (and hence also nocov) inputchecks first for newdata
  callNextMethod()

  period.last <- Cov.Date <- NULL

  # prediction.end needs to be ok to work with it
  check_err_msg(check_user_data_predictionend(clv.fitted=clv.fitted, prediction.end=prediction.end))

  err.msg <- c()

  # Check that dyncov covariate is long enough for prediction end
  #   Convert prediction.end already for this
  #   newdata will replace existing data therefore check its cov
  #     Also clv.time in newdata has to be used for conversion of prediction.end
  dt.predictions <- clv.time.get.prediction.table(clv.time = user.newdata@clv.time,
                                                 user.prediction.end = prediction.end)

  tp.last.required.cov.period <- clv.time.floor.date(clv.time = user.newdata@clv.time,
                                                     timepoint = dt.predictions[1, period.last])

  # only need to check one cov data, guaranteed that both are same length
  if(tp.last.required.cov.period > user.newdata@data.cov.trans[, max(Cov.Date)])
    err.msg <- c(err.msg, "The dynamic covariates in parameter newdata are not long enough for the given parameter prediction.end!")

  check_err_msg(err.msg)
})


# . clv.controlflow.predict.check.inputs ------------------------------------------------------------------------
#' @importFrom methods callNextMethod
setMethod(f = "clv.controlflow.predict.check.inputs", signature = signature(clv.fitted="clv.fitted.transactions.dynamic.cov"), function(clv.fitted, verbose, uncertainty, num.boots, level, prediction.end, continuous.discount.factor, predict.spending, ...){
  # Do static cov (and hence also nocov) inputchecks first
  #   After this, newdata is basically ok
  callNextMethod()

  period.last <- Cov.Date <- NULL

  err.msg <- c()

  # Check that dyncov covariate is long enough for prediction end
  #   Convert prediction.end already for this
  dt.predictions <- clv.time.get.prediction.table(clv.time = clv.fitted@clv.data@clv.time,
                                                 user.prediction.end = prediction.end)
  tp.last.required.cov.period <- clv.time.floor.date(clv.time = clv.fitted@clv.data@clv.time,
                                                     timepoint = dt.predictions[1, period.last])

  # only need to check one cov data, guaranteed that both are same length
  if(tp.last.required.cov.period > clv.fitted@clv.data@data.cov.trans[, max(Cov.Date)])
    err.msg <- c(err.msg, "The dynamic covariates in the fitted model are not long enough for the given parameter prediction.end!")

  check_err_msg(err.msg)
})



# . clv.controlflow.predict.new.customer ------------------------------------------------------------------------
#' @include class_clv_fitted_transactions_dynamiccov.R
setMethod(f = "clv.controlflow.predict.new.customer", signature = signature(clv.fitted="clv.fitted.transactions.dynamic.cov"), definition = function(clv.fitted, clv.newcustomer){
  Cov.Date <- NULL

  check_err_msg(check_user_data_predict_newcustomer_dyncov(clv.fitted=clv.fitted, clv.newcustomer=clv.newcustomer))

  # There is no required minimum prediction period. In the tracking plot,
  # the unconditional expectation is always used for <= 2 (or 3) periods

  # Convert time point data and verify it correctness
  # This can only be done here once the clv.time object is known

  # readability
  clv.time <- clv.fitted@clv.data@clv.time

  # convert time information to native date/time types
  clv.newcustomer <- clv.newcustomer.dynamic.cov.convert.time(
    clv.newcustomer=clv.newcustomer, clv.time=clv.time)

  tp.first.transaction <- clv.newcustomer@first.transaction
  tp.prediction.end <- tp.first.transaction + clv.time.number.timeunits.to.timeperiod(
    clv.time=clv.time, user.number.periods=clv.newcustomer@num.periods)

  check_err_msg(check_user_data_newcustomer_dyncovspecific(
    clv.time=clv.time,
    dt.cov.life=clv.newcustomer@data.cov.life,
    dt.cov.trans=clv.newcustomer@data.cov.trans,
    tp.first.transaction=tp.first.transaction,
    tp.prediction.end=tp.prediction.end))


  return(clv.model.predict.new.customer.unconditional.expectation(
    clv.model = clv.fitted@clv.model,
    clv.fitted = clv.fitted,
    t=clv.newcustomer@num.periods,
    clv.newcustomer=clv.newcustomer))
})
