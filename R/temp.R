#' Stress Index
#'
#' @description
#' Uses a precedure (e.g. Principal Components) to create a "stress index".
#'
#' @param ValData. xts object. Required. This is the input used for prediction.
#' @param ValDates List of (or vector of) pairs of vectors of begin and end date-time ranges.  Required (if not all of the ValidationData is to be used).
#' @param proc String. Required. Default is "pca" (Principal Component Analysis)
#' @param TrainData xts object. Required.
#' @param TrainDates List of (or vector of) pairs of vectors of begin and end date-time ranges.  Required (if not all of the ValidationData is to be used).
#' @param TestData xts object. Required (if any Testing (Tuning) is required.
#' @param TestDates List of (or vector of) pairs of vectors of begin and end date-time ranges.  Required (if not all of the ValidationData is to be used).
#' @return Prediction.
#' @examples
#' \dontrun{
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
stressIndex <- function(ValData, ValDates, proc = "pca",
                        TrainData, TrainDates,
                        TestData,  TestDates,
                        ...
) {
tryCatchLog::tryCatchLog({
  stop("Nothing here yet.")
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


#' Re-Createion of the St. Louis Federal Board of Governors Financial Stress Index Version 2
#'
#' @description
#' Uses a precedure (e.g. Principal Components) to create the "stress index" Financial Stress Index Version 2
#'
#' @references
#' \cite{St. Louis Fed Financial Stress Index (STLFSI2)
#' \url{https://fred.stlouisfed.org/series/STLFSI2}
#' }
#' @references
#' cite\{author(s)?, Table 1: Data Used to Construct STLFSI
#' \url{https://fredblog.stlouisfed.org/wp-content/uploads/2020/03/FSI2.pdf}
#' }
#' @references
#' \cite{Kevin Kliesen and Michael McCracken, The St. Louis Fed’s Financial Stress Index, Version 2.0, March 26, 2020
#' \url{https://fredblog.stlouisfed.org/2020/03/the-st-louis-feds-financial-stress-index-version-2-0/}
#' }
#' @references
#' \cite{List of Data Series Used to Construct the St. Louis Fed Financial Stress Index (STLFSI)
#'   \url{https://www.stlouisfed.org/~/media/Files/PDFs/STLFSI-Key.pdf?la=en}
#' }
#' @references
#' \cite{list of the components that are used to construct the STLFSI
#'   \url{https://www.stlouisfed.org/news-releases/st-louis-fed-financial-stress-index/stlfsi-key}
#' }
#' @references
#' \cite{St. Louis Fed Financial Stress Index (DISCONTINUED) (STLFSI)
#' \url{https://fred.stlouisfed.org/series/STLFSI}
#' }
#' @references
#' \cite{author(s)?, National Economic Trends, Appendix, January 2010
#' \url{https://files.stlouisfed.org/files/htdocs/publications/net/NETJan2010Appendix.pdf}
#' }
#' @references
#' \cite{Kevin L. Kliesen and Douglas C. Smith, Measuring Financial Market Stress, Economic SYNOPSES, 2010 Number 2, Posted on January 15, 2010
#' \url{https://files.stlouisfed.org/files/htdocs/publications/es/10/ES1002.pdf}
#' }
#' @param ValData xts object. Required. This is the input used for prediction.
#' @param ValDates List of (or vector of) pairs of vectors of begin and end date-time ranges.  Required (if not all of the ValidationData is to be used).
#' @param TrainData xts object. Required.
#' @param TrainDates List of (or vector of) pairs of vectors of begin and end date-time ranges.  Required (if not all of the ValidationData is to be used).
#' @return Prediction.
#' @examples
#' \dontrun{
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
FSI2 <- function(ValData = ValData, ValDates = ValDates,
                 TrainData = TrainData, TrainDates = TrainDates,
                 ...
) {
tryCatchLog::tryCatchLog({

  # STLFSI TrainData
  # We construct the STLFSI using 18 weekly data series . . .
  #
  # STLFSI2
  # 18 weekly data series, all of which are weekly averages of daily data series

  # STLFSI2
  # The key difference between versions 1.0 and 2.0 is that
  # 2.0 uses daily changes in interest rates and stock prices,
  # rather than the levels of interest rates and stock prices in the PCA calculation.


  # STLFSI, s of 07/15/2010 the Vanguard Financial Exchange-Traded Fund series
  # has been replaced with the S&P 500 Financials Index.

  # STLFSI TrainDates
  # . . . over the
  # sample period December 31, 1993, to December 11, 2009
  #
  # STLFSI and STLFSI2 start December 31, 1993
  # STLFSI ends March 13, 2020
  # # The St. Louis Fed’s Financial Stress Index, Version 2.0
  # https://fredblog.stlouisfed.org/2020/03/the-st-louis-feds-financial-stress-index-version-2-0/

  # BUT THEY MAY BE DOING
  # When the sample changes (i.e., a new week of data is added),
  # the values of the FSI in the original sample can be changed.
  #


  # STLFSI proc
  # Principal components analysis is used to construct the STLFSI
  #
  # STLFSI index
  # by extracting this factor (the first principal component)
  # we are able to create an index
  #

  # STLFSI index reports
  # St. Louis Fed Financial Stress Index (DISCONTINUED) (STLFSI)
  # Weekly, Ending Friday
  # https://fred.stlouisfed.org/series/STLFSI
  #
  # St. Louis Fed Financial Stress Index (STLFSI2)
  # Weekly, Ending Friday
  # https://fred.stlouisfed.org/series/STLFSI2


  # STLFSI index intepretation
  # How to Interpret the Index:
  # The average value of the index, which begins in late 1993, is designed to be zero.
  # Thus, zero is viewed as representing normal financial market conditions.
  # Values below zero suggest below-average financial market stress,
  # while values above zero suggest above-average financial market stress.


  #
  #   STLFSI proc details
  #   1. First, each of the data series is de-meaned.
  #      The de-meaned series are then
  #      divided by their respective sample standard deviations (SDs)
  #      (Because each variable was standardized,
  #      the coefficient of a variable (SEE 2.) represents the
  #      "influence of a 1 SD change in that variable on the FSI.")
  #   2. we use the method of principal components to calculate the coefficients of the variables
  #   3. We then scale these coefficients so that the SD of the index is 1.
  #   4. Finally, each data series is multiplied by its respective adjusted coefficient
  #   5. FSI for time t is
  #      the sum of each series multiplied by its respective adjusted coefficient.
  #      (Higher values of the FSI indicate a
  #     greater degree of financial stress in the economy.)
  #
  #   STLFSI proc details drawbacks
  #     A negative coefficient multiplied by a negative data value
  #     will result in a positive contribution to financial stress.
  #
  #   STLFSI percent of the total variation
  #     in the 18 variables (DEFINITION)
  #     1 – SSE/SST
  #


  stressIndex(ValData, ValDates, proc = "pca",
              TrainData, TrainDates,
              ...
  )

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}
