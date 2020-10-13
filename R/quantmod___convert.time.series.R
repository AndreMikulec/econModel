
#' Convert a time series to an R CRAN package quantmod supported time series
#'
#' Used internally by the function getSymbols.ALFRED
#'
#' Replica of quantmod:::convert.time.series
#'
#' Code is repeated here because it is private code in the R CRAN package quantmod.  CRAN rules do not allow private functions in other packages to be called directly.
#'
#' @param fr see R CRAN package quantmod private function convert.time.series
#' @param return.class see R CRAN package quantmod private function convert.time.series
#' @importFrom stats as.ts
#' @importFrom timeSeries timeSeries
quantmod___convert.time.series <- function (fr, return.class)
{
  if ("quantmod.OHLC" %in% return.class) {
    class(fr) <- c("quantmod.OHLC", "zoo")
    return(fr)
  }
  else if ("xts" %in% return.class) {
    return(fr)
  }
  if ("zoo" %in% return.class) {
    return(as.zoo(fr))
  }
  else if ("ts" %in% return.class) {
    fr <- stats::as.ts(fr)
    return(fr)
  }
  else if ("data.frame" %in% return.class) {
    fr <- as.data.frame(fr)
    return(fr)
  }
  else if ("matrix" %in% return.class) {
    fr <- as.data.frame(fr)
    return(fr)
  }
  else if ("timeSeries" %in% return.class) {
    if (requireNamespace("timeSeries", quietly = TRUE)) {
      fr <- timeSeries::timeSeries(coredata(fr), charvec = as.character(index(fr)))
      return(fr)
    }
    else {
      warning(paste("'timeSeries' from package 'timeSeries' could not be loaded:",
                    " 'xts' class returned"))
    }
  }
}
