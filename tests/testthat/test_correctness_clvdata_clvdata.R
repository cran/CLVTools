data("cdnow")
data("apparelTrans") # for years

# estimation.split ---------------------------------------------------------------------


# **clv.time: predict(, 10). regexp 10 Weeks + regexp holdout.start
#     same as if with dates, also different tz / formats
#     prediction.end=14.4 all equal prediction.end=14. same with dates ("1997-12-26" vs "1997-12-31")

#
test_that("Different estimation.split formats result in same split - data in Dates", {
  skip_on_cran()
  cdnow.date <- data.table::copy(cdnow)
  cdnow.date[, Date := as.Date(Date)]

  apparelTrans.date <- data.table::copy(apparelTrans)
  apparelTrans.date[, Date := as.Date(Date)]

  # fct.helper.correctness.estimationsplit(data = cdnow.date, estimation.number = 37*7*24, estimation.char = "1997-09-17",
  #                                         time.unit = "hours")

  fct.helper.correctness.estimationsplit(data = cdnow.date, estimation.number = 37*7,
                                         estimation.char = as.character(cdnow.date[, min(Date)+(37*7)]),
                                         # estimation.char = as.character(cdnow[, clv.time.floor.date(clv.time.days("ymd"), min(Date))]+lubridate::period(37*7, "days")),
                                         time.unit = "days")

  fct.helper.correctness.estimationsplit(data = cdnow.date, estimation.number = 37,
                                         estimation.char = as.character(cdnow[, min(Date)]+lubridate::period(37, "weeks")),
                                         time.unit = "weeks")

  #Not feasible with the current sample data (too short).
  #fct.helper.correctness.estimationsplit(data = apparelTrans.date, estimation.number = 1,
  #                                       estimation.char = as.character(apparelTrans.date[, min(Date)]+lubridate::period(1, "year")),
  #                                       time.unit = "years")
})


test_that("Different estimation.split formats result in same split - data in POSIX UTC", {
  cdnow.posix <- data.table::copy(cdnow)
  cdnow.posix[, Date := lubridate::parse_date_time(Date ,orders = "ymd", tz = "UTC")]

  apparelTrans.posix <- data.table::copy(apparelTrans)
  apparelTrans.posix[, Date := lubridate::parse_date_time(Date ,orders = "ymd", tz = "UTC")]


  # fct.helper.correctness.estimationsplit(data = cdnow.posix, estimation.number = 37*7*24, estimation.char = "1997-09-17",
  #                                         time.unit = "hours")

  fct.helper.correctness.estimationsplit(data = cdnow.posix, estimation.number = 37*7, estimation.char = "1997-09-17",
                                         time.unit = "days", warn=TRUE)

  fct.helper.correctness.estimationsplit(data = cdnow.posix, estimation.number = 37,
                                         estimation.char = as.character(cdnow[, min(Date)]+lubridate::period(37, "weeks")),
                                         # estimation.char = "1997-09-17",
                                          time.unit = "weeks", warn=TRUE)
  #Not feasible with the current sample data (too short).
  #fct.helper.correctness.estimationsplit(data = apparelTrans.posix, estimation.number = 1,
  #                                       estimation.char = as.character(apparelTrans.posix[, min(Date)]+lubridate::period(1, "year")),
  #                                        time.unit = "years", warn=TRUE)

})

test_that("Different estimation.split formats result in same split - data in POSIX ACST", {

  cdnow.posix <- data.table::copy(cdnow)
  cdnow.posix[, Date := lubridate::parse_date_time(Date ,orders = "ymd", tz = "Australia/Darwin")]

  apparelTrans.posix <- data.table::copy(apparelTrans)
  apparelTrans.posix[, Date := lubridate::parse_date_time(Date ,orders = "ymd", tz = "Australia/Darwin")]

  # fct.helper.correctness.estimationsplit(data = cdnow.posix, estimation.number = 37*7*24, estimation.char = "1997-09-17",
  #                                         time.unit = "hours")

  fct.helper.correctness.estimationsplit(data = cdnow.posix, estimation.number = 37*7, estimation.char = "1997-09-17",
                                         time.unit = "days", warn=TRUE)

  fct.helper.correctness.estimationsplit(data = cdnow.posix, estimation.number = 37,
                                         estimation.char = as.character(cdnow[, min(Date)]+lubridate::period(37, "weeks")),
                                         time.unit = "weeks", warn=TRUE)

  #Not feasible with the current sample data (too short).
  #fct.helper.correctness.estimationsplit(data = apparelTrans.posix, estimation.number = 1,
  #                                       estimation.char = as.character(apparelTrans.posix[, min(Date)]+lubridate::period(1, "year")),
  #                                        time.unit = "years", warn=TRUE)

})


test_that("Different estimation.split formats result in same split - data in char", {
  skip_on_cran()

  cdnow.char <- data.table::copy(cdnow)
  cdnow.char[, Date := as.character(Date)]

  apparelTrans.char <- data.table::copy(apparelTrans)
  apparelTrans.char[, Date := as.character(Date)]

  # fct.helper.correctness.estimationsplit(data = cdnow.char, estimation.number = 37*7*24, estimation.char = "1997-09-17",
  #                                         time.unit = "hours")

  fct.helper.correctness.estimationsplit(data = cdnow.char, estimation.number = 37*7,
                                         estimation.char = "1997-09-17",
                                         time.unit = "days")

  fct.helper.correctness.estimationsplit(data = cdnow.char, estimation.number = 37,
                                         # estimation.char = "1997-09-17",
                                         estimation.char = as.character(cdnow[, min(Date)]+lubridate::period(37, "weeks")),
                                         time.unit = "weeks")
  #Not feasible with the current sample data (too short).
  #fct.helper.correctness.estimationsplit(data = apparelTrans.char, estimation.number = 1,
  #                                       estimation.char = as.character(apparelTrans[, min(Date)]+lubridate::period(1, "years")),
  #                                       time.unit = "years")

})


test_that("No estimation.split ends on last transaction date", {
  skip_on_cran()
  expect_silent(clv.data.cdnow <- clvdata(time.unit = "w", data.transactions = cdnow, date.format = "ymd"))
  expect_equal(clv.data.cdnow@clv.time@timepoint.estimation.start, cdnow[,min(Date)])
  expect_equal(clv.data.cdnow@clv.time@timepoint.estimation.end, cdnow[, max(Date)])
  expect_equal(clv.data.cdnow@clv.time@timepoint.holdout.start, cdnow[, max(Date)])
  expect_equal(clv.data.cdnow@clv.time@timepoint.holdout.end, cdnow[, max(Date)])
})

# time.unit ------------------------------------------------------------------------------

test_that("Different units with same split results in same dates", {
  #skip_on_cran()

  # numeric breaks with floordate for estimation.start
  # expect_silent(data.days <-  clvdata(estimation.split = 37*7, time.unit = "days",
  #                                     data.transactions = cdnow, date.format = "ymd"))
  # expect_silent(data.weeks <- clvdata(estimation.split = 37, time.unit = "weeks",
  #                                     data.transactions = cdnow, date.format = "ymd"))
  # expect_silent(data.hours <- clvdata(estimation.split = 37*7*24, time.unit = "hours",
  #                                     data.transactions = cdnow, date.format = "ymd"))

  expect_silent(data.days <-  clvdata(estimation.split = "1997-09-17", time.unit = "days",
                                      data.transactions = cdnow, date.format = "ymd"))
  expect_silent(data.weeks <- clvdata(estimation.split = "1997-09-17", time.unit = "weeks",
                                      data.transactions = cdnow, date.format = "ymd"))

  expect_equal(data.weeks@clv.time@timepoint.estimation.end,   data.days@clv.time@timepoint.estimation.end)
  expect_equal(data.weeks@clv.time@timepoint.estimation.start,data.days@clv.time@timepoint.estimation.start)
  expect_equal(data.weeks@clv.time@timepoint.holdout.start,    data.days@clv.time@timepoint.holdout.start)
  expect_equal(data.weeks@clv.time@timepoint.holdout.end,      data.days@clv.time@timepoint.holdout.end)
})



# transaction.data ----------------------------------------------------------------------------

test_that("Same result for differently sorted transactions", {
  skip_on_cran()
  cdnow.shuffle   <- data.table::copy(cdnow)[sample.int(n = nrow(cdnow)), ]

  expect_silent(data.normal  <- clvdata(cdnow,         date.format = "ymd", time.unit = "w"))
  expect_silent(data.shuffle <- clvdata(cdnow.shuffle, date.format = "ymd", time.unit = "w"))

  data.normal@call  <- data.shuffle@call <- as.symbol("abc")

  expect_equal(data.normal, data.shuffle)
})


test_that("Same results for different Id formats", {
  skip_on_cran()

  cdnow.char      <- data.table::copy(cdnow)
  cdnow.factor    <- data.table::copy(cdnow) # often when loading data
  cdnow.numeric   <- data.table::copy(cdnow)
  cdnow.char[,    Id := as.character(Id)]
  cdnow.numeric[, Id := as.numeric(Id)]
  cdnow.factor[,  Id := as.factor(as.character(Id))]

  expect_silent(data.char    <- clvdata(cdnow.char,    date.format = "ymd", time.unit = "w"))
  expect_silent(data.numeric <- clvdata(cdnow.numeric, date.format = "ymd", time.unit = "w"))
  expect_silent(data.factor  <- clvdata(cdnow.factor,  date.format = "ymd", time.unit = "w"))

  data.char@call    <- as.symbol("abc")
  data.numeric@call <- as.symbol("abc")
  data.factor@call  <- as.symbol("abc")

  expect_equal(data.char, data.factor)
  expect_equal(data.factor, data.numeric)
})

test_that("Same results for different Price formats", {
  skip_on_cran()

  cdnow.integer   <- data.table::copy(cdnow) # often when loading data
  cdnow.numeric   <- data.table::copy(cdnow)

  cdnow.numeric[, Price := as.numeric(as.integer(Price))] # make integer first to cutoff digits
  cdnow.integer[, Price := as.integer(Price)]

  expect_silent(data.numeric <- clvdata(cdnow.numeric, date.format = "ymd", time.unit = "w"))
  expect_silent(data.integer <- clvdata(cdnow.integer, date.format = "ymd", time.unit = "w"))

  data.numeric@call <- as.symbol("abc")
  data.integer@call <- as.symbol("abc")

  expect_equal(data.numeric, data.integer)
})



test_that("No price data gives correct object", {
  skip_on_cran()

  expect_silent(clv.cdnow <- clvdata(cdnow, date.format = "ymd", time.unit = "w", estimation.split = 37, name.price = NULL))
  expect_false(clv.cdnow@has.spending)
  expect_false(clv.data.has.spending(clv.cdnow))
  expect_false("Price" %in% colnames(clv.cdnow@data.transactions))
  expect_false("Price" %in% colnames(clv.cdnow@data.repeat.trans))
  # also nothing in descriptives
  expect_true(nrow(summary(clv.cdnow)$descriptives.transactions[like(pattern = "Spending", vector = Name)])==0)
})

test_that("Price data gives correct object", {
  skip_on_cran()
  expect_silent(clv.cdnow <- clvdata(cdnow, date.format = "ymd", time.unit = "w", estimation.split = 37, name.price = "Price"))
  expect_true(clv.cdnow@has.spending)
  expect_true(clv.data.has.spending(clv.cdnow))
  expect_true("Price" %in% colnames(clv.cdnow@data.transactions))
  expect_true("Price" %in% colnames(clv.cdnow@data.repeat.trans))
  # also Spending info in descriptives
  expect_true(nrow(summary(clv.cdnow)$descriptives.transactions[like(pattern = "Spending", vector = Name)]) > 0)
})



# test_that: same results for different Date formats is already checked very often in estimation.split

test_that("Transaction data was properly copied", {
  skip_on_cran()
  expect_silent(clv.data.cdnow <- clvdata(data.transactions = cdnow,time.unit = "weeks", date.format = "ymd"))
  expect_false(isTRUE(all.equal(data.table::address(clv.data.cdnow@data.transactions),
                                data.table::address(cdnow))))
})


# aggregate.transactions ----------------------------------------------------------------------------

test_that("Only one transaction per timepoint and Id exits (date)", {
  skip_on_cran()
  expect_silent(dt.trans <- data.table(Id =   c("1", "1", "1", "2", "2", "2"),
                                       Date = c(lubridate::ymd("2019-01-01"), lubridate::ymd("2019-01-01"), lubridate::ymd("2019-01-01"),
                                                lubridate::ymd("2019-06-01"), lubridate::ymd("2019-06-01"), lubridate::ymd("2019-06-02"))))
  expect_silent(dt.trans.correct <- data.table(Id =   c( "1", "2","2"),
                                               Date = c(lubridate::ymd("2019-01-01"),
                                                        lubridate::ymd("2019-06-01"), lubridate::ymd("2019-06-02"))))
  expect_silent(dt.trans.agg <- clv.data.aggregate.transactions(dt.transactions = dt.trans, has.spending = FALSE))
  expect_true(fsetequal(dt.trans.agg, dt.trans.correct))
})

test_that("Only one transaction per timepoint and Id exits (posix)", {
  skip_on_cran()
  expect_silent(dt.trans <- data.table(Id =   c("1", "1", "1", "2", "2", "2"),
                                       Date = c(lubridate::ymd("2019-01-01"), lubridate::ymd("2019-01-01"), lubridate::ymd("2019-01-01"),
                                                lubridate::ymd("2019-06-01"), lubridate::ymd("2019-06-01"), lubridate::ymd("2019-06-02"))))
  expect_silent(dt.trans.correct <- data.table(Id =   c( "1", "2","2"),
                                               Date = c(lubridate::ymd("2019-01-01"),
                                                        lubridate::ymd("2019-06-01"), lubridate::ymd("2019-06-02"))))
  expect_silent(dt.trans[, Date := as.POSIXct(Date)])
  expect_silent(dt.trans.correct[, Date := as.POSIXct(Date)])

  expect_silent(dt.trans.agg <- clv.data.aggregate.transactions(dt.transactions = dt.trans, has.spending = FALSE))
  expect_true(fsetequal(dt.trans.agg, dt.trans.correct))

})

test_that("Same timepoint transactions are summed (date)", {
  expect_silent(dt.trans <- data.table(Id =   c("1", "1", "1", "2", "2", "2"),
                                       Date = c(lubridate::ymd("2019-01-01"), lubridate::ymd("2019-01-01"), lubridate::ymd("2019-01-01"),
                                                lubridate::ymd("2019-06-01"), lubridate::ymd("2019-06-01"), lubridate::ymd("2019-06-02")),
                                       Price = c(1, 2, 3,
                                                 4, 5, 6)))
  expect_silent(dt.trans.correct <- data.table(Id =   c( "1", "2","2"),
                                               Date = c(lubridate::ymd("2019-01-01"),
                                                        lubridate::ymd("2019-06-01"), lubridate::ymd("2019-06-02")),
                                               Price = c(1+2+3,
                                                         4+5,6)))
  expect_silent(dt.trans.agg <- clv.data.aggregate.transactions(dt.transactions = dt.trans, has.spending = TRUE))
  expect_true(fsetequal(dt.trans.agg, dt.trans.correct))
})

test_that("Same timepoint transactions are summed (posix)", {
  expect_silent(dt.trans <- data.table(Id =   c("1", "1", "1", "2", "2", "2"),
                                       Date = c(lubridate::ymd("2019-01-01"), lubridate::ymd("2019-01-01"), lubridate::ymd("2019-01-01"),
                                                lubridate::ymd("2019-06-01"), lubridate::ymd("2019-06-01"), lubridate::ymd("2019-06-02")),
                                       Price = c(1, 2, 3,
                                                 4, 5, 6)))
  expect_silent(dt.trans.correct <- data.table(Id =   c( "1", "2","2"),
                                               Date = c(lubridate::ymd("2019-01-01"),
                                                        lubridate::ymd("2019-06-01"), lubridate::ymd("2019-06-02")),
                                               Price = c(1+2+3,
                                                         4+5,6)))
  expect_silent(dt.trans[, Date := as.POSIXct(Date)])
  expect_silent(dt.trans.correct[, Date := as.POSIXct(Date)])

  expect_silent(dt.trans.agg <- clv.data.aggregate.transactions(dt.transactions = dt.trans, has.spending = TRUE))
  expect_true(fsetequal(dt.trans.agg, dt.trans.correct))
})

# repeat.transactions ----------------------------------------------------------------------------

test_that("Removes correct transaction",{
  # Correctly remove first transaction, regardless of sorting
  expect_silent(dt.trans <- data.table(Date = lubridate::ymd(c("2019-01-01", "2019-01-02", "2019-01-03", "2019-06-01", "2019-06-02")),
                                       Id = c("1", "1", "1", "2", "2")))
  expect_silent(dt.trans.correct <- data.table(Date = lubridate::ymd(c("2019-01-02", "2019-01-03","2019-06-02")),
                                               Id = c( "1", "1", "2")))

  # Ordered by Date
  # Order one way
  expect_silent(dt.repeat.trans <- clv.data.make.repeat.transactions(dt.trans[order(Date)]))
  expect_true(fsetequal(dt.repeat.trans, dt.trans.correct))
  # Order other way
  expect_silent(dt.repeat.trans <- clv.data.make.repeat.transactions(dt.trans[order(-Date)]))
  expect_true(fsetequal(dt.repeat.trans, dt.trans.correct))

  # Ordered by Id
  # One way
  setorderv(dt.trans, cols = "Id", order = 1)
  expect_silent(dt.repeat.trans <- clv.data.make.repeat.transactions(dt.trans))
  expect_true(fsetequal(dt.repeat.trans, dt.trans.correct))
  # Other way
  setorderv(dt.trans, cols = "Id", order = -1)
  expect_silent(dt.repeat.trans <- clv.data.make.repeat.transactions(dt.trans))
  expect_true(fsetequal(dt.repeat.trans, dt.trans.correct))
})

test_that("2 at the same timepoint (Date)", {
  # 2 on same date, remove only 1
  expect_silent(dt.trans <- data.table(Date = c(lubridate::ymd("2019-01-01"), lubridate::ymd("2019-01-01"), lubridate::ymd("2019-01-02"),
                                                lubridate::ymd("2019-06-01"), lubridate::ymd("2019-06-01")),
                                       Id =   c("1", "1", "1", "2", "2")))
  expect_silent(dt.trans.correct <- data.table(Date = c(lubridate::ymd("2019-01-01"), lubridate::ymd("2019-01-02"),
                                                        lubridate::ymd("2019-06-01")),
                                               Id =   c( "1", "1", "2")))

  expect_silent(dt.repeat.trans <- clv.data.make.repeat.transactions(dt.trans))
  expect_true(fsetequal(dt.repeat.trans, dt.trans.correct))
})

test_that("2 at the same timepoint (posix)", {
  # 2 on same date, remove only 1
  expect_silent(dt.trans <- data.table(Date = c(lubridate::ymd_hms("2019-01-01 00:00:01"), lubridate::ymd_hms("2019-01-01 00:00:01"), lubridate::ymd_hms("2019-01-02 00:00:01"),
                                                lubridate::ymd_hms("2019-06-01 00:00:01"), lubridate::ymd_hms("2019-06-01 00:00:01")),
                                       Id =   c("1", "1", "1", "2", "2")))
  expect_silent(dt.trans.correct <- data.table(Date = c(lubridate::ymd_hms("2019-01-01 00:00:01"), lubridate::ymd_hms("2019-01-02 00:00:01"),
                                                        lubridate::ymd_hms("2019-06-01 00:00:01")),
                                               Id =   c( "1", "1", "2")))

  expect_silent(dt.repeat.trans <- clv.data.make.repeat.transactions(dt.trans))
  expect_true(fsetequal(dt.repeat.trans, dt.trans.correct))
})

test_that("Zero-repeaters are removed", {
  expect_silent(dt.trans <- data.table(Date = c(lubridate::ymd("2019-01-01"), lubridate::ymd("2019-01-02"), lubridate::ymd("2019-01-01")),
                                       Id =   c("1", "1", "2")))
  expect_silent(dt.repeat.trans <- clv.data.make.repeat.transactions(dt.trans))
  expect_false("2" %in% dt.repeat.trans$Id)
  expect_true("1" %in% dt.repeat.trans$Id)
})



test_that("Aggregating first and removing after removes all first transactions", {
  expect_silent(dt.trans <- data.table(Id =   c("1", "1", "1", "2", "2", "2"),
                                       Date = c(lubridate::ymd("2019-01-01"), lubridate::ymd("2019-01-01"), lubridate::ymd("2019-01-02"),
                                                lubridate::ymd("2019-06-01"), lubridate::ymd("2019-06-01"), lubridate::ymd("2019-06-02")),
                                       Price = c(1, 2, 3,
                                                 4, 5, 6)))
  expect_silent(dt.trans.correct <- data.table(Id =   c( "1", "2"),
                                               Date = c(lubridate::ymd("2019-01-02"),
                                                        lubridate::ymd("2019-06-02")),
                                               Price = c(3,
                                                         6)))

  expect_silent(dt.agg.repeat.trans <- clv.data.make.repeat.transactions(clv.data.aggregate.transactions(dt.trans, has.spending = TRUE)))
  expect_true(fsetequal(dt.agg.repeat.trans, dt.trans.correct))

  expect_silent(clv.d <- clvdata(dt.trans, date.format = "ymd", time.unit = "w", estimation.split = NULL))
  expect_true(fsetequal(clv.d@data.repeat.trans, dt.trans.correct))
})


test_that("clv.data.get.repeat.transactions.in.estimation.period() is the same as creating repeat transactions from estimation period data", {
  skip_on_cran()

  clv.cdnow <- clvdata(cdnow, date.format = "ymd", time.unit = "w", estimation.split = NULL)

  dt.estimation.period   <- clv.data.get.transactions.in.estimation.period(clv.cdnow)
  dt.self.made.repeat    <- clv.data.make.repeat.transactions(dt.estimation.period)

  expect_true(fsetequal(clv.data.get.repeat.transactions.in.estimation.period(clv.cdnow),
                        dt.self.made.repeat))
})


# mean.interpurchase.times ----------------------------------------------------------------------------
test_that("New faster mean.interpurchase.times same result as original implementation", {

  clv.cdnow <- fct.helper.create.clvdata.cdnow(data.cdnow=cdnow)

  fct.old.interp.time <- function(clv.data, dt.transactions){

    num.transactions <- dt.transactions[, list(num.trans = .N), by="Id"]

    return(rbindlist(list(
      # 1 Transaction = NA
      dt.transactions[Id %in% num.transactions[num.trans == 1,Id], list(interp.time = NA_real_, Id)],
      dt.transactions[Id %in% num.transactions[num.trans >  1,Id],
                      list(interp.time = mean(clv.time.interval.in.number.tu(clv.time = clv.data@clv.time,
                                                                             interv = lubridate::int_diff(Date)))),
                      by="Id"]
    ), use.names = TRUE))
  }

  # Old method implicitly depended on input data being sorted by (Id, Date)
  dt.old <- fct.old.interp.time(
    clv.data = clv.cdnow,
    dt.transactions = clv.cdnow@data.transactions
    )

  # Sort by Price instead for new to show it is also correct if input is not
  # ordered by Id, Date
  dt.new <- clv.data.mean.interpurchase.times(
    clv.data = clv.cdnow,
    dt.transactions = clv.cdnow@data.transactions[order(Price)]
    )
  # Need to sort by Id because result order depends on order of input data
  expect_equal(
    object = dt.new[order(Id)],
    expected = dt.old[order(Id)],
    tolerance = testthat_tolerance()
    )



})
