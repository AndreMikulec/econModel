% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSymbols.ALFRED.R
\name{getSymbols.YaleU}
\alias{getSymbols.YaleU}
\title{S&P 500 Stock Market Data from Yale University}
\usage{
getSymbols.YaleU(
  Symbols,
  env,
  return.class = "xts",
  returnIndex = "ObservationDate",
  ...
)
}
\arguments{
\item{Symbols}{A character vector specifying the names of each symbol to be loaded.
Possible Symbols are the following:
"YaleUStockMarketData" (means get all of the columns) xor
one specific column name. e.g. "CAPE"}

\item{env}{where to create objects. (.GlobalEnv)}

\item{return.class}{desired class of returned object.
Can be xts, zoo, data.frame, or xts (default)}

\item{returnIndex}{one of "ObservationDate" (row element date) or "LastUpdatedDate" (dates of estimations of in cases such that estimations are done?). Default is ObservationDate".  Note, an 'observation date'(row element date) is  not the 'date of estimation'. The 'observation date' (typically) observes the beginning of the 'date range' (its period: ObservationDate + Frequency).  The LastUpdatedDate date, that is, the date of estimation, is after the the period has completed, that is after  ObservationDate + Frequency.}

\item{...}{Dots passed.}
}
\value{
A call to getSymbols.YaleU will load into the specified
environment one object for each \code{Symbol} specified,
with class defined by \code{return.class}.
}
\description{
Robert Shillers S&P 500 Stock Market Data from his Excel spreadsheet as a R CRAN package quantmod function getSymbols source (src).
}
\examples{
\dontrun{
# common usage
getSymbols(c("CAPE", "Dividends", "Earnings"), src = "YaleU")

# all columns in one xts object
getSymbols("YaleUstockMarketData", src = "YaleU")
}
}
\references{
\cite{Online Data of Robert Shiller
\url{http://www.econ.yale.edu/~shiller/data.htm}}

\cite{Home Page of Robert J. Shiller - Yale University
\url{http://www.econ.yale.edu/~shiller/}}
}
\author{
Andre Mikulec (adapted original code to work with Yale University stock market data)
}
