
#' Download US Treasury Yield Curve Data
#'
#' @description
#' Downloads US Treasury yield curve data from the US Treasury web site.
#' This is a fix and partial re-write of R CRAN package ustyc function getYieldCurve.
#'
#' Omitted columns are like "^.*_DATE.*$" (because many character vector of many different date formats exist) and  like "^CUSIP_.*$" (and these are alphanumeric).
#' The omit-ion of columns most affects DailyTreasuryBillRateData.
#'
#' Note, this is not "tuned for performance".  Tuning may be appropriate for a getSymbols.TreasuryYields. (Perhaps, this would be a future project.)
#'
#' @param base String. Required. The base URL for the data service, defaulting to http://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData. If the month or year arguments are not NULL, then the function modifies this URL to parameterize the download request.
#' @param year The desired year number or NULL for all years (default)
#' @param month	 The desired month number or NULL for all months (default).
#' @return xts object
#' @author Andre Mikulec (partial re-write)
#' @author Matt Barry (original)
#' @references
#' \cite{GetYieldCurve - XML content does not seem to be XML #1
#' \url{https://github.com/mrbcuda/ustyc/issues/1}
#' }
#' @references
#' \cite{Unknown IO error #2
#' \url{https://github.com/mrbcuda/ustyc/issues/2}
#' }
#' @examples
#' \dontrun{
#'
#' # https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yield
#' xlist <- getYieldCurve("https://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData?$filter=month(NEW_DATE)%20eq%2012%20and%20year(NEW_DATE)%20eq%202020")
#' xlist
#'            BC_1MONTH BC_2MONTH BC_3MONTH BC_6MONTH BC_1YEAR BC_2YEAR BC_3YEAR BC_5YEAR BC_7YEAR
#' 2020-12-01      0.07      0.07      0.09      0.10     0.12     0.17     0.22     0.42     0.68
#' 2020-12-02      0.07      0.08      0.09      0.10     0.11     0.16     0.22     0.42     0.69
#' 2020-12-03      0.08      0.08      0.08      0.09     0.10     0.16     0.21     0.40     0.67
#' 2020-12-04      0.07      0.07      0.09      0.10     0.11     0.16     0.21     0.42     0.70
#'            BC_10YEAR BC_20YEAR BC_30YEAR BC_30YEARDISPLAY
#' 2020-12-01      0.92      1.46      1.66             1.66
#' 2020-12-02      0.95      1.50      1.70             1.70
#' 2020-12-03      0.92      1.46      1.67             1.67
#' 2020-12-04      0.97      1.53      1.73             1.73
#'
#'
#' # https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=billrates
#' xlist <- getYieldCurve("https://data.treasury.gov/feed.svc/DailyTreasuryBillRateData?$filter=month(INDEX_DATE)%20eq%2012%20and%20year(INDEX_DATE)%20eq%202020")
#' xlist
#'            ROUND_B1_CLOSE_4WK_2 ROUND_B1_YIELD_4WK_2 ROUND_B1_CLOSE_8WK_2 ROUND_B1_YIELD_8WK_2
#' 2020-12-01                 0.07                 0.07                 0.07                 0.07
#' 2020-12-02                 0.07                 0.07                 0.08                 0.08
#' 2020-12-03                 0.08                 0.08                 0.08                 0.08
#' 2020-12-04                 0.07                 0.07                 0.07                 0.07
#'            ROUND_B1_CLOSE_13WK_2 ROUND_B1_YIELD_13WK_2 ROUND_B1_CLOSE_26WK_2 ROUND_B1_YIELD_26WK_2
#' 2020-12-01                  0.09                  0.09                  0.10                  0.10
#' 2020-12-02                  0.09                  0.09                  0.10                  0.10
#' 2020-12-03                  0.08                  0.08                  0.09                  0.09
#' 2020-12-04                  0.09                  0.09                  0.10                  0.10
#'            ROUND_B1_CLOSE_52WK_2 ROUND_B1_YIELD_52WK_2 BOND_MKT_UNAVAIL_REASON CS_4WK_CLOSE_AVG
#' 2020-12-01                  0.12                  0.12                      NA       0.07000000
#' 2020-12-02                  0.11                  0.11                      NA       0.07000000
#' 2020-12-03                  0.10                  0.10                      NA       0.07333333
#' 2020-12-04                  0.11                  0.11                      NA       0.07250000
#'            CS_4WK_YIELD_AVG CS_8WK_CLOSE_AVG CS_8WK_YIELD_AVG CS_13WK_CLOSE_AVG CS_13WK_YIELD_AVG
#' 2020-12-01       0.07000000       0.07000000       0.07000000        0.09000000        0.09000000
#' 2020-12-02       0.07000000       0.07500000       0.07500000        0.09000000        0.09000000
#' 2020-12-03       0.07333333       0.07666667       0.07666667        0.08666667        0.08666667
#' 2020-12-04       0.07250000       0.07500000       0.07500000        0.08750000        0.08750000
#'            CS_26WK_CLOSE_AVG CS_26WK_YIELD_AVG CS_52WK_CLOSE_AVG CS_52WK_YIELD_AVG CF_WEEK
#' 2020-12-01        0.10000000        0.10000000             0.120             0.120  202049
#' 2020-12-02        0.10000000        0.10000000             0.115             0.115  202049
#' 2020-12-03        0.09666667        0.09666667             0.110             0.110  202049
#' 2020-12-04        0.09750000        0.09750000             0.110             0.110  202049
#'
#' # https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=longtermrate
#' xlist <- getYieldCurve(base = "http://data.treasury.gov/feed.svc/DailyTreasuryLongTermRateData?$filter=month(QUOTE_DATE)%20eq%2012%20and%20year(QUOTE_DATE)%20eq%202020")
#' xlist
#'            BC_20year Over_10_Years Real_Rate
#' 2020-12-01      1.46          1.47     -0.39
#' 2020-12-02      1.50          1.51     -0.39
#' 2020-12-03      1.46          1.47     -0.42
#' 2020-12-04      1.53          1.54     -0.38
#'
#' # https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=realyield
#' xlist <- getYieldCurve("https://data.treasury.gov/feed.svc/DailyTreasuryRealYieldCurveRateData?$filter=month(NEW_DATE)%20eq%2012%20and%20year(NEW_DATE)%20eq%202020")
#' xlist
#'             TC_5YEAR  TC_7YEAR TC_10YEAR TC_20YEAR TC_30YEAR
#' 2020-12-01 -1.300375 -1.106995 -0.889312 -0.493451 -0.299437
#' 2020-12-02 -1.335888 -1.129040 -0.897425 -0.488592 -0.293812
#' 2020-12-03 -1.384003 -1.176739 -0.936774 -0.511666 -0.322141
#' 2020-12-04 -1.408367 -1.177114 -0.923189 -0.484069 -0.278824
#'
#' # Since 2000
#' # Long Term Real Rate Average: The Long-Term Real Rate Average is the
#' # unweighted average of bid real yields on all outstanding TIPS
#' # with remaing maturities of more than 10 years and is intended as a
#' # proxy for long-term real rates.
#' # https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=reallongtermrate
#' xlist <- getYieldCurve("https://data.treasury.gov/feed.svc/DailyTreasuryRealLongTermRateAverageData?$filter=month(QUOTE_DATE)%20eq%2012%20and%20year(QUOTE_DATE)%20eq%202020")
#' xlist
#'             RATE
#' 2020-12-01 -0.39
#' 2020-12-02 -0.39
#' 2020-12-03 -0.42
#' 2020-12-04 -0.38
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom curl curl_fetch_memory
#' @importFrom XML xmlParse
#' @importFrom XML xmlToList
#' @importFrom data.table rbindlist
#' @importFrom zoo index
#' @importFrom xts as.xts
#' @export
getYieldCurve <- function (base = "http://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData",
                           year = NULL, month = NULL){
tryCatchLog::tryCatchLog({

  oldtz <- Sys.getenv("TZ")
  if(oldtz!="UTC") {
    Sys.setenv(TZ="UTC")
  }

  location <- base
  yloc <- mloc <- doc <- NULL
  yloc <- if (!is.null(year))
    paste0("year(NEW_DATE)%20eq%20", year)
  mloc <- if (!is.null(month))
    paste0("month(NEW_DATE)%20eq%20", month)
  parameters <- ""
  if (!is.null(yloc)  && !is.null(mloc)) {
    parameters = paste0("?$filter=", mloc, "%20and%20", yloc)
  }
  else {
    if (!is.null(yloc))
      parameters = paste("?$filter=", yloc, sep = "")
    if (!is.null(mloc))
      parameters = paste("?$filter=", mloc, sep = "")
  }

  req <- curl::curl_fetch_memory(paste(location, parameters, sep = ""))
  # XML object
  doc <- XML::xmlParse(rawToChar(req$content))

  if (is.null(doc)) {
    warning(paste("Could not parse the location.", location))
    return(NULL)
  }
  message("Download and parse complete.  Converting to list...")
  x <- XML::xmlToList(doc)
  message("List conversion complete.  Converting to frame...")
  updated = x[["updated"]]
  x <- x[!Names(x) %in% c("title", "id", "updated", "link",".attrs" )]
  y <- lapply(x, function(xx) {
    Iter <- 0L
    yy <- data.frame(lapply(xx$content$properties, function(xxx) {
      if(is.list(xxx)) {
        if(length(xxx$text)) {
          text <- xxx$text
        } else {
          text <- NA # logical
        }
      } else {
        if(length(xxx)) {
          text <- xxx
        } else {
          text <- NA # logical
        }
      }
      if(is.character(text) && text == "N/A") {
        text <- NA_character_
      }
      return(text)
    }))

    # see before destruction
    # print("_Before_Destruction_")
    # print(str(yy))

    # need to split an xts into a set: (Hmisc) can help

    # 1st entry remove (column 1)
    yy[grepl( "^.*Id$", Names(yy))] <- NULL
    # removed

    # 2nd entry is the *DATE
    colnames(yy)[1] <- "DATE"
    # renamed
    yy[["DATE"]] <- as.POSIXct(yy[["DATE"]])
    # re-classed

    # remove all other "^.*_DATE.*$" # many date/time formats
    yy[grepl( "^.*_DATE.*$", colnames(yy))] <- NULL

    # remove all other "^CUSIP_.*$"
    yy[grepl("^CUSIP_.*$", colnames(yy))] <- NULL

    yy
  })
  # data.frame is y

  message("Row bind conversion beginning.")
  y <- data.table::rbindlist(y)
  message("Row bind conversion complete.")

  rownames(y) <- NULL

  # remainder convert to real numbers
  y <- data.frame(mapply(function(yy, colname) {
    # save the factor column (if any)
    Done <- FALSE
    # The one and only
    if(!Done && colname == "DATE") {
       yy <- as.POSIXct(yy); Done <- T
    }
    # everything else (except FACTORs)
    if(!Done && !grepl("^.*_TYPE$", colname)) {
       yy <- as.numeric(yy); Done <- T
    }
    # leave alone factors: "^.*_TYPE$"
    yy
  }, y, colnames(y), SIMPLIFY = F))
  message("Frame conversion complete.")

  # it has a factor, so I cast
  if(any(grepl("^.*_TYPE$", colnames(y)))) {
    message("xcast beginning.")
    y <- xcast(y, IndexVar = "DATE", ValueVar = "RATE")
    # xts object is out
    message("xcast complete.")
  } else {
    # index
    rownames(y) <- y[["DATE"]]; y[["DATE"]] <- NULL
    y <- xts::as.xts(y)
  }

  Sys.setenv(TZ=oldtz)

  y
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}
