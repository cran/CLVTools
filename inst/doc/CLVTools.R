## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  #fig.path = "figures/WALKTHROUGH-",
  out.width = "100%"
)

# On CRAN, code may not run with more than 2 threads
# Otherwise results in failed check with "Re-building vignettes had CPU time xxx times elapsed time"
data.table::setDTthreads(threads = 2)

## ----install-package-CRAN, eval = FALSE---------------------------------------
#  install.packages("CLVTools")

## ----install-package-GITHUB, eval = FALSE-------------------------------------
#  install.packages("devtools")
#  devtools::install_github("bachmannpatrick/CLVTools", ref = "development")

## ----load-library-------------------------------------------------------------
library("CLVTools")

## ----load-data----------------------------------------------------------------
data("apparelTrans")
apparelTrans

## ----load-CreateObj-----------------------------------------------------------
clv.apparel <- clvdata(apparelTrans,  
                       date.format="ymd", 
                       time.unit = "week",
                       estimation.split = 40,
                       name.id = "Id",
                       name.date = "Date",
                       name.price = "Price")

## ----print-CLVObject----------------------------------------------------------
clv.apparel

## ----summary-CLVObject--------------------------------------------------------
summary(clv.apparel)

## ----estimate-model-formula---------------------------------------------------
   est.pnbd <- latentAttrition(formula = , family = pnbd, data=clv.apparel)
   est.pnbd

## ----estimate-model-formula2, eval=FALSE--------------------------------------
#    est.pnbd <- latentAttrition(formula = , family = pnbd, data=clv.apparel,
#                                optimx.args = list(control=list(trace=5),
#                                         method="Nelder-Mead"),
#                                start.params.model=c(r=1, alpha=10, s=2, beta=8))

## ----estimate-model, eval=FALSE-----------------------------------------------
#      est.pnbd <- pnbd(clv.data = clv.apparel)
#      est.pnbd

## ----estimate-model2, eval=FALSE----------------------------------------------
#     est.pnbd <- pnbd(clv.data = clv.apparel,
#                      start.params.model = c(r=1, alpha = 2, s = 1, beta = 2),
#                      optimx.args = list(control=list(trace=5),
#                                         method="Nelder-Mead"
#                                         ))

## ----param-summary------------------------------------------------------------
#Full detailed summary of the parameter estimates
summary(est.pnbd)

#Extract the coefficients only
coef(est.pnbd)
#Alternative: oefficients(est.pnbd.obj)


## ----coef-model---------------------------------------------------------------
#Extract the coefficients only
coef(est.pnbd)
#Alternative: oefficients(est.pnbd.obj)

#Extract the confidence intervals
confint(est.pnbd)


## ----ll-model-----------------------------------------------------------------
# LogLikelihood at maximum
logLik(est.pnbd)

# Variance-Covariance Matrix at maximum
vcov(est.pnbd)


## ----estimate-ggomnbd-formula, eval=FALSE-------------------------------------
#    est.ggomnbd <- latentAttrition(formula = , family = ggomnbd, data=clv.apparel,
#                                   optimx.args = list(method="Nelder-Mead")
#                                   start.params.model=c(r=0.7, alpha=5, b=0.005,  s=0.02, beta=0.001))

## ----estimate-ggomnbd, eval=FALSE---------------------------------------------
#  est.ggomnbd <- ggomnbd(clv.data = clv.apparel,
#                         start.params.model = c(r=0.7, alpha=5, b=0.005,  s=0.02, beta=0.001),
#                         optimx.args = list(method="Nelder-Mead"))

## ----predict-model------------------------------------------------------------
results <- predict(est.pnbd)
print(results)


## ----predict-model2, eval = FALSE---------------------------------------------
#  predict(est.pnbd, prediction.end = 30)

## ----plot-model3, eval = FALSE------------------------------------------------
#  predict(est.pnbd, prediction.end = "2006-05-08")

## ----plot-actual, fig.height=4.40, fig.width=9--------------------------------
plot(clv.apparel)


## ----plot-interpurchase, fig.height=4.40, fig.width=9-------------------------
plot(clv.apparel, which="interpurchasetime")


## ----plot-model, fig.height=4.40, fig.width=9---------------------------------
plot(est.pnbd)


## ----plot-model2, eval = FALSE------------------------------------------------
#  plot(est.pnbd, prediction.end = 30, cumulative = TRUE)

## ----predict-model3, eval = FALSE---------------------------------------------
#  plot(est.pnbd, prediction.end = "2006-05-08", cumulative = TRUE)

## ----predict-model4, eval = FALSE---------------------------------------------
#  plot(est.pnbd, which="pmf", trans.bins=0:5, label.remaining="6+")

## ----Cov-staticData-----------------------------------------------------------
data("apparelStaticCov")
apparelStaticCov

## ----Cov-dynData--------------------------------------------------------------
data("apparelDynCov")
apparelDynCov

## ----Cov-setStatic------------------------------------------------------------
clv.static<- SetStaticCovariates(clv.data = clv.apparel, 
                                      data.cov.life = apparelStaticCov, 
                                      data.cov.trans = apparelStaticCov,
                                      names.cov.life = c("Gender", "Channel"), 
                                      names.cov.trans =c("Gender", "Channel"), 
                                      name.id = "Id")

## ----Cov-setDynamic, eval=FALSE, message=FALSE, warning=TRUE------------------
#  clv.dyn <- SetDynamicCovariates(clv.data = clv.apparel,
#                                       data.cov.life = apparelDynCov,
#                                       data.cov.trans = apparelDynCov,
#                                       names.cov.life = c("Marketing", "Gender", "Channel"),
#                                       names.cov.trans = c("Marketing", "Gender", "Channel"),
#                                       name.id = "Id",
#                                       name.date = "Cov.Date")

## ----static-cov-estimate-formula1, message=TRUE, warning=FALSE, eval=FALSE----
#    est.pnbd.static <- latentAttrition(formula = ~ .|., family = pnbd, data=clv.static)

## ----static-cov-estimate-formula2, warning=FALSE, eval=FALSE------------------
#    est.pnbd.static <- latentAttrition(formula = ~ Gender|Channel+Gender,
#                                       family = pnbd, data=clv.static)
#  

## ----static-covariates-formula3, warning=FALSE, eval=FALSE--------------------
#    est.pnbd.static <- latentAttrition(formula = ~ Channel+Gender|I(log(Channel+2)),
#                                       family = pnbd, data=clv.static)

## ----dyn-cov-formula1, eval=FALSE---------------------------------------------
#  est.pnbd.dyn <- latentAttrition(formula = ~ .|., family = pnbd, data = clv.dyn,
#                                  optimx.args = list(control=list(trace=5)))

## ----static-cov-estimate, message=TRUE, warning=FALSE-------------------------
est.pnbd.static <- pnbd(clv.static, 
                         start.params.model = c(r=1, alpha = 2, s = 1, beta = 2),
                         start.params.life = c(Gender=0.6, Channel=0.4),
                         start.params.trans = c(Gender=0.6, Channel=0.4))

## ----dyn-cov-estimate, eval=FALSE---------------------------------------------
#  est.pnbd.dyn <- pnbd(clv.dyn,
#                       start.params.model = c(r=1, alpha = 2, s = 1, beta = 2),
#                       start.params.life = c(Marketing=0.5, Gender=0.6, Channel=0.4),
#                       start.params.trans = c(Marketing=0.5, Gender=0.6, Channel=0.4),
#                       optimx.args = list(control=list(trace=5)))

## ----Cov-summary--------------------------------------------------------------
summary(est.pnbd.static)

## ----cor-formula1, eval=FALSE-------------------------------------------------
#  est.pnbd.cor <- latentAttrition(formula = , family = pnbd,
#                                  use.cor=TRUE, data=clv.apparel)

## ----Cov-cor, eval=FALSE------------------------------------------------------
#  est.pnbd.cor <- pnbd(clv.apparel,
#                       use.cor= TRUE)
#  summary(est.pnbd.cor)

## ----reg-formula, eval=FALSE--------------------------------------------------
#  est.pnbd.reg <- latentAttrition(formula = ~ .|., family = pnbd,
#                                  reg.lambdas=c(life=3, trans=8), data=clv.static)
#  summary(est.pnbd.reg)

## ----reg-advOptions, eval=FALSE-----------------------------------------------
#  est.pnbd.reg <- pnbd(clv.static,
#                           start.params.model = c(r=1, alpha = 2, s = 1, beta = 2),
#                           reg.lambdas = c(trans=100, life=100))
#  summary(est.pnbd.reg)

## ----constr-formula, eval=FALSE-----------------------------------------------
#  est.pnbd.constr <- latentAttrition(formula = ~.|., family = pnbd, data = clv.static,
#                                     names.cov.constr=c("Gender"),
#                                     start.params.constr = c(Gender = 0.6))
#  summary(est.pnbd.constr)

## ----constr-advOptions, eval=FALSE--------------------------------------------
#  est.pnbd.constr <- pnbd(clv.static,
#                           start.params.model = c(r=1, alpha = 2, s = 1, beta = 2),
#                           start.params.constr = c(Gender=0.6),
#                           names.cov.constr=c("Gender"))
#  summary(est.pnbd.constr)

## ----spending-load-data and initialize----------------------------------------
data("apparelTrans")
apparelTrans

clv.apparel <- clvdata(apparelTrans,  
                       date.format="ymd", 
                       time.unit = "week",
                       estimation.split = 40,
                       name.id = "Id",
                       name.date = "Date",
                       name.price = "Price")

## ----spending-estimate-formula1-----------------------------------------------
est.gg <- spending(family = gg, data=clv.apparel)
est.gg

## ----spending-estimate-formula2, eval=FALSE-----------------------------------
#  est.gg <- spending(family = gg, data=clv.apparel,
#                     optimx.args = list(control=list(trace=5)),
#                     start.params.model=c(p=0.5, q=15, gamma=2))

## ----spending-estimate-formula3, eval=FALSE-----------------------------------
#  est.gg <- spending(family = gg, data=clv.apparel,
#                     remove.first.transaction=FALSE)

## ----spending-estimate-model1, eval=FALSE-------------------------------------
#  est.gg<- gg(clv.data = clv.apparel)
#  est.gg

## ----spending-estimate-model2, eval=FALSE-------------------------------------
#  est.gg<- gg(start.params.model=c(p=0.5, q=15, gamma=2), clv.data = clv.apparel)
#  est.gg

## ----spending-estimate-model3, eval=FALSE-------------------------------------
#  est.gg<- gg(clv.data = clv.apparel, remove.first.transaction=FALSE)
#  est.gg

## ----spending-predict-model---------------------------------------------------
results.spending <- predict(est.gg)
print(results.spending)

## ----spending-plot-model4, fig.height=4.40, fig.width=9-----------------------
plot(est.gg)


