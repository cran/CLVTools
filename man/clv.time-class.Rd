% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_clv_time.R, R/f_s3generics_clvtime.R
\docType{class}
\name{clv.time-class}
\alias{clv.time-class}
\alias{show,clv.time-method}
\title{Time Unit defining conceptual periods}
\usage{
\S4method{show}{clv.time}(object)
}
\description{
Represents a time unit and offers methods for time unit related functionality.
It stores all information related to timepoints (i.e. time and dates) of the
estimation and holdout sample periods and offers methods to parse user input
and to do 'time-unit math'.
}
\details{
This encapsulation of all time unit functionality in one class allows for custom type of
time units such as Bi-weekly or irregularly spaced time units.

\code{clv.time} is a virtual class and sub-classes implement the actual parsing and calculations.

\linkS4class{clv.time.date} uses data type \code{Date} for time units equal or
greater than a single day that do not require a time of day.

\linkS4class{clv.time.datetime} uses data type \code{POSIXct} for
time units smaller than a single day.
}
\section{Slots}{

\describe{
\item{\code{timepoint.estimation.start}}{Single \code{Date} or \code{POSIXct} that stores the start of the estimation period.}

\item{\code{timepoint.estimation.end}}{Single \code{Date} or \code{POSIXct} that stores the end of the estimation period.}

\item{\code{timepoint.holdout.start}}{Single \code{Date} or \code{POSIXct} that stores the start of the holdout period.}

\item{\code{timepoint.holdout.end}}{Single \code{Date} or \code{POSIXct} that stores the end of the holdout period.}

\item{\code{time.format}}{Single character vector with the format that is used to parse dates and times given as characters.}

\item{\code{estimation.period.in.tu}}{Single numeric indicating the length of the estimation period in number of time units.}

\item{\code{holdout.period.in.tu}}{Single numeric indicating the length of the holdout period in number of time units.}

\item{\code{name.time.unit}}{Single character vector storing the human-readable name of the time unit for output.}
}}

\seealso{
\link{summary.clv.time} for a summary about an object of class \code{clv.time}

\linkS4class{clv.time.days} for an implementation of time unit 'Days'

\linkS4class{clv.time.weeks} for an implementation of time unit 'Weeks'

\linkS4class{clv.time.years} for an implementation of time unit 'Years'
}
\keyword{internal}
