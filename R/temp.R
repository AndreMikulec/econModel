
#' Download US Treasury Yield Curve Data
#'
#' @description
#' Downloads US Treasury yield curve data from the US Treasury web site.
#' This is a fix and partial re-write of R CRAN package ustyc function getYieldCurve.
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
#' # https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=longtermrate
#' xlist <- getYieldCurve(base = "http://data.treasury.gov/feed.svc/DailyTreasuryLongTermRateData?$filter=month(QUOTE_DATE)%20eq%2012%20and%20year(QUOTE_DATE)%20eq%202020")
#' xlist
#'            BC_20year Over_10_Years Real_Rate
#' 2020-12-01      1.46          1.47     -0.39
#' 2020-12-02      1.50          1.51     -0.39
#' 2020-12-03      1.46          1.47     -0.42
#' 2020-12-04      1.53          1.54     -0.38
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

    yy <- data.frame(lapply(xx$content$properties, function(xxx) {

      if(is.list(xxx) && length(xxx$text)) {
        text <- xxx$text
      } else {
        text <- xxx
      }
      if(text == "N/A") {
        text <- NA_character_
      }
      return(text)
    }))

    # remove
    yy[grepl( "^.*Id$", Names(yy))] <- NULL

    # 2nd entry is the *DATE (somewhere, there, always is a _DATE)
    colnames(yy)[grepl( "^.*_DATE$", Names(yy))] <- "DATE"
    yy[["DATE"]] <- as.POSIXct(yy[["DATE"]])

    # remove (if any)
    yy[grepl( "^.*_FACTOR$", Names(yy))] <- NULL

    yy
  })
  # data.frame is y

  message("Row bind conversion beginning.")
  y <- data.table::rbindlist(y)
  message("Row bind conversion complete.")

  rownames(y) <- NULL

  # remainder convert to real numbers
  y <- data.frame(mapply(function(yy,colname) {
    # save the factor column (if any)
    if(!grepl("^.*_TYPE$", colname) && !colname == "DATE") {
        as.numeric(yy)
    } else {
        yy
    }
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
