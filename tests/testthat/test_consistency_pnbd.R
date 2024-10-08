skip_on_cran()

data("cdnow")
data("apparelTrans")
data("apparelStaticCov")
data("apparelDynCov")

fct.testthat.consistency(name.model = "PNBD", method = pnbd,
                         data.apparelTrans = apparelTrans, data.apparelStaticCov=apparelStaticCov,
                         fct.LL.ind.nocov = pnbd_nocov_LL_ind, fct.LL.ind.static.cov = pnbd_staticcov_LL_ind)

# nocov vs dyncov --------------------------------------------------------------------------------------
#   same predict with gamma=0

p.nocov <- fit.apparel.nocov()


# . Prepare dyncov: Set parameters -------------------------------------------------------------------
# Fake the parameters to be exactly the same and 0 for covariates
#   Replace model coefs with that from nocov
fct.helper.dyncov.g0.with.predition.params.model <- function(p.dyncov, prediction.params.model){

  expect_silent(p.dyncov@prediction.params.model[c("r", "alpha", "s", "beta")] <- prediction.params.model[c("r", "alpha", "s", "beta")])

  expect_silent(p.dyncov@prediction.params.life[] <- 0)
  expect_silent(p.dyncov@prediction.params.trans[] <- 0)

  # Recalculate the LL data for these fake params
  expect_silent(log.params <- setNames(log(p.dyncov@prediction.params.model[c("r", "alpha", "s", "beta")]),
                                       c("log.r", "log.alpha", "log.s", "log.beta")))
  expect_silent(log.params[c("life.Marketing", "life.Gender", "life.Channel", "trans.Marketing", "trans.Gender", "trans.Channel")] <- 0)
  expect_silent(p.dyncov@LL.data <- pnbd_dyncov_getLLdata(clv.fitted=p.dyncov, params=log.params))
  return(p.dyncov)
}

p.dyncov.g0 <- fct.helper.dyncov.quickfit.apparel.data(data.apparelTrans = apparelTrans, data.apparelDynCov = apparelDynCov)
p.dyncov.g0 <- fct.helper.dyncov.g0.with.predition.params.model(p.dyncov = p.dyncov.g0, prediction.params.model = p.nocov@prediction.params.model[c("r", "alpha", "s", "beta")])


# . Dyncov: Compare nocov vs dyncov LL ---------------------------------------------------------------
fct.testthat.consistency.cov.params.0.predict.same(fitted.nocov = p.nocov,
                                                   fitted.cov.g0 = p.dyncov.g0,
                                                   is.dyncov = TRUE)

fct.testthat.consistency.cov.params.0.plot.same(fitted.nocov = p.nocov,
                                                fitted.cov.g0 = p.dyncov.g0)

test_that("predict newcustomer dyncov same results for all models with gamma=0", {
  # requires newcustomer.dynamic() while `fct.testthat.consistency.cov.params.0.predict.newcustomer.same()`
  # uses newcustomer.static()
  df.cov <- fct.helper.default.newcustomer.covdata.dyncov()

  expect_silent(nc.pred.nocov <- predict(
    p.nocov,
    newdata=newcustomer(num.periods = 7.89),
    verbose=FALSE))
  expect_silent(nc.pred.g0 <- predict(
    p.dyncov.g0,
    newdata=newcustomer.dynamic(
      num.periods = 7.89,
      data.cov.life = df.cov,
      data.cov.trans = df.cov,
      first.transaction = '2000-01-13'),
    verbose=FALSE))

  expect_equal(nc.pred.nocov, nc.pred.g0)
})


# compare LL
test_that("Dyncov LL same as nocov for alpha==beta and alpha!=beta", {
  skip_on_cran()

  .fct.nocov.LL.for.params <- function(params.model){
    expect_silent(log.params.nocov <- setNames(log(params.model[p.nocov@clv.model@names.original.params.model]),
                                               p.nocov@clv.model@names.prefixed.params.model))
    expect_silent(l.args.nocov <- list(vLogparams = log.params.nocov,
                                       vX = p.nocov@cbs$x, vT_x = p.nocov@cbs$t.x, vT_cal = p.nocov@cbs$T.cal))
    expect_silent(LL.ind.nocov <- do.call(pnbd_nocov_LL_ind, l.args.nocov))
    return(drop(LL.ind.nocov))
  }

  # Alpha != beta
  stopifnot(coef(p.nocov)["beta"] != coef(p.nocov)["alpha"])
  LL.ind.nocov.alpha.neq.beta <- .fct.nocov.LL.for.params(params.model = coef(p.nocov))
  expect_true(isTRUE(all.equal(p.dyncov.g0@LL.data[order(Id)]$LL, LL.ind.nocov.alpha.neq.beta)))

  # Alpha == beta
  model.params.a.eq.b <- coef(p.nocov)
  model.params.a.eq.b[c("alpha", "beta")] <- 1.234
  LL.ind.nocov.alpha.eq.beta <- .fct.nocov.LL.for.params(params.model = model.params.a.eq.b)
  p.dyncov.g0.a.eq.b <- fct.helper.dyncov.g0.with.predition.params.model(p.dyncov = p.dyncov.g0, prediction.params.model = model.params.a.eq.b)
  expect_true(isTRUE(all.equal(p.dyncov.g0.a.eq.b@LL.data[order(Id)]$LL, LL.ind.nocov.alpha.eq.beta)))
})




