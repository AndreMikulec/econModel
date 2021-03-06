% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/machineLearning.R
\name{tradeModelSummary}
\alias{tradeModelSummary}
\title{Calculates performance across resamples}
\usage{
tradeModelSummary(data, lev = NULL, model = NULL)
}
\arguments{
\item{data}{Data.frame. Required. A data frame with columns, obs and pred, for the observed and predicted outcomes. For metrics that rely on class probabilities, such as twoClassSummary, columns should also include predicted probabilities for each class. See the classProbs argument to R CRAN package caret function trainControl.}

\item{lev}{Character vector of factors levels for the response. In regression cases, this would be NULL.  . Default is NULL.}

\item{model}{String. Default is NULL.  Required. A character string for the model name (as taken from the method argument of train).}
}
\value{
profit
}
\description{
Given two numeric vectors, obs and pred, of data, the performance is calculated.
}
\examples{
\dontrun{
options(tradeModelSummaryDots = list(signal.threshold = c(0,0)))

model <- caret::train(dat[,-1], dat[,1], metric="profit", maximize=TRUE,
  trControl = caret::trainControl(summaryFunction = tradeModelSummary)
)
}
}
\references{
\cite{Zachary Mayer, Time series cross-validation 5, January 24, 2013
\url{https://www.r-bloggers.com/2013/01/time-series-cross-validation-5/}
}
}
