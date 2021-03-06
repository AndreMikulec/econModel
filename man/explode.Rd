% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explode.R
\name{explode}
\alias{explode}
\title{expland out a two dimensional object}
\usage{
explode(
  x1 = NULL,
  x2 = NULL,
  Fun = NULL,
  Flags = NULL,
  AltName = NULL,
  asIsAltName = NULL,
  Prefix = NULL,
  FixedSep = NULL,
  quote = FALSE,
  envir = parent.frame(2),
  ...
)
}
\arguments{
\item{x1}{two dimensional object}

\item{x2}{Optionally, second two dimensional object.}

\item{Fun}{function name in the "bare" or in literal quotes("")}

\item{Flags}{list of possible varying parameters that are expanded to all possible combinations by expand.grid}

\item{AltName}{string alternate name for "Fun"}

\item{asIsAltName}{Default is NULL(F). Passed to the column generator function.  NULL is interpreted as F.
This NULL(F) means do not attempt to simplify the AltName.
For example, if the user passed "TTR::SMA" and asIsAltName == T, then "TTR::SMA" would
directly tried to become part of the column name (and "::" would cause an error so this
would not work.).  If the user passed "TTR::SMA" and asIsAltName == NULL(F), then
"SMA" would be directly tried to directly become part of the column name.}

\item{Prefix}{boolan default is FALSE.  TRUE would place the column meta before the column name.}

\item{FixedSep}{string divider of meta items}

\item{quote}{boolean passed to package DescTools function DoCall}

\item{envir}{calling environment}

\item{...}{additional parameters}
}
\value{
two dimensional object with new and different columns
}
\description{
From one(1) or two(2) two dimension objects, apply a function(3) upon the two objects.
Return a two dimensional object of with a column name derived from the three.
Functions are meant to be many of the functions from R CRAN packages package TTR and PerformanceAnalytics.
Note, if any x2, then x1 and x2 are paired/matched column position to column position.
Note, this the "multivariate form" of the "single variate form" object TTR function data generator from the web page "Time series cross-validation 5".
}
\examples{
\dontrun{

library(quantmod)
ibm <- getSymbols("IBM", from = "1970-01-01", to = "1970-01-13", auto.assign = FALSE)

explode(ibm[,c("IBM.Open")], Fun = "TTR::SMA", Flags = list(n = 2:3))
          IBM.Open.TTR.SMA.n.2  IBM.Open.TTR.SMA.n.3
1970-01-02                   NA                   NA
1970-01-05               18.262                   NA
1970-01-06               18.356               18.312
1970-01-07               18.419               18.379
1970-01-08               18.431               18.425
1970-01-09               18.456               18.446
1970-01-12               18.463               18.454

explode(ibm[,c("IBM.Open","IBM.Close")], Fun = "TTR::SMA", Flags = list(n = 2:3))

           IBM.Open.TTR.SMA.n.2 IBM.Close.TTR.SMA.n.2 IBM.Open.TTR.SMA.n.3
1970-01-02                   NA                    NA                   NA
1970-01-05               18.262                18.325                   NA
1970-01-06               18.356                18.419               18.312
1970-01-07               18.419                18.431               18.379
1970-01-08               18.431                18.456               18.425
1970-01-09               18.456                18.463               18.446
1970-01-12               18.463                18.419               18.454

# column naming checks

explode(ibm[,c("IBM.Open","IBM.Close")], Fun = TTR::SMA, Flags = list(n = 2:3),
        AltName = "CUST")
IBM.Open.CUST.n.2 IBM.Close.CUST.n.2 IBM.Open.CUST.n.3 IBM.Close.CUST.n.3

explode(IBM.Open.TTR.SMA.n.2, Fun = TTR::SMA, Flags = list(n = 3:4))
IBM.Open.TTR.SMA.n.2.TTR.SMA.n.3 IBM.Open.TTR.SMA.n.2.TTR.SMA.n.4

SMA2 <- TTR::SMA
explode(IBM.Open.TTR.SMA.n.2, Fun = SMA2, Flags = list(n = 3:4))
IBM.Open.TTR.SMA.n.2.SMA2.n.3 IBM.Open.TTR.SMA.n.2.SMA2.n.4

# x2 case

# R CRAN Package TTR function runCor
# runCor : function (x, y, n = 10, use = "all.obs", sample = TRUE, cumulative = FALSE)
explode(ibm[,c("IBM.Open","IBM.Close")], ibm[,c("IBM.Low","IBM.High")],
        Fun = "TTR::runCor", Flags = list(n = 4:5, sample = c(TRUE,FALSE)))

}
}
\references{
\cite{Zachary Mayer, "Time series cross-validation 5"
\url{http://www.r-bloggers.com/time-series-cross-validation-5/}
\url{http://moderntoolmaking.blogspot.com/2013/01/time-series-cross-validation-5.html}
}
}
