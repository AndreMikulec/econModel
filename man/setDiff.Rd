% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toPeriod.R
\name{setDiff}
\alias{setDiff}
\title{setdiff any dimension-less  object}
\usage{
setDiff(x, y)
}
\arguments{
\item{x}{vector}

\item{y}{vector of elements to subtract off from x}
}
\value{
vector from x, elements of y have been subtracted from x
}
\description{
\preformatted{

Unlike base::setdiff does not do "as.vector".
Therefore vector classes are not lost.
( e.g. Date and POSIXt).

To compare, it uses (same as setdiff.default) match.

}
}
