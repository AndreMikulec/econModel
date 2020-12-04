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

  # STLFSI TrainDates
  # We construct the STLFSI using 18 weekly data series . . .
  #
  # STLFSI TrainData
  # . . . over the
  # sample period December 31, 1993, to December 11, 2009

  stressIndex(ValData, ValDates, proc = "pca",
              TrainData, TrainDates,
              ...
  )

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}
