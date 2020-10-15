
#' Download Federal Reserve Economic Data - ALFRED(R)
#'
#' R access to thousands of data series accessible via the St. Louis Federal Reserve Bank's ALFRED system.
#'
#' Downloads Symbols to specified env from 'research.stlouisfed.org'. This method is not to be called directly, instead a call to R CRAN packages quantmod function getSymbols(Symbols,src='ALFRED') will in turn call this method.
#'
#' The St. Louis Federal Reserve Bank's FRED allows a specific data series to be revised(overwritten) with different data while using the same published tail date of 'Date Range'.  This revision is called the 2nd vintage. This function collects and displays data from the 1st vintage that only contains non-revised data (that means the original data) per published tail date of 'Date Range'.
#'
#' @param Symbols a character vector specifying the names of each symbol to be loaded
#' @param env where to create objects. (.GlobalEnv)
#' @param return.class class of returned object
#' @param earlLastUpdDate character or Date.  Earliest date that is before or 'at' the vintage 'Last Updated' date in the past that a user may wish to query upon. Default is null (no restriction).  This is useful in the situation when the user already owns prior data, and just wants just some recent data.  Internally, this just subtracts off some 'Last Updated' dates from the results of calling the function getVintages.
#' @param vintages.per.query number of vintages per HTTPS GET. A.k.a the number of vintages per sheet.   Default is 12.  Common maximum is 12. Value can be "Max". Practical experience has performed with 192.  The maximum may be different during different times of the day or night.  This parameter exists to enhance performance by limiting the number of trips to the server.
#' @param look.back how deep in periods to look back for the latest observation in all of the non-oldest vintages.  Meant to use with datasets with a wide range of time between the Measurement interval and the Validity interval.  From the 'Last Updated' date try to peek back in time to the 1st vintage with a published tail 'Date Range' date that is within variable 'look.back' periods. If the periodicy is "day" and, just after a three(3) day holiday weekend, to reach back from a Tuesday to a Friday, parameter look.back is increased to a minimum value of 4.  Default is 3.  Increase this value if much time exists between the tail date of 'Date Range' and the 'Last Updated' date: meaning zero(0) observations exist in the look.back period.  The R CRAN package xts function periodicity determines the period of time.  This function is meant to minimize CPU and disk I/O.
#' @param fullOldestVintageData if TRUE, then also return the oldest vintage data and keep(prepend) its data.  Default is FALSE. Useful when 'as much data as possible' is important.
#' @param datasheet if TRUE, then also return all of the vintages in an xts attribute 'datasheet'. Default is FALSE.  Useful for debugging.
#' @param allowParallel if TRUE, then collect groups of 'sheets of vintages.per.query vintages' in parallel.  Default is FALSE.  (Improved) performance will vary: this is more useful on (more data points) weekly data or daily data. Because this is a server side activity, the number of parallel processes does NOT depend on the local machine CPUs.
#' @param ... additional parameters
#'
#' @author Andre Mikulec   (adapted from the original code)
#' @author Jeffrey A. Ryan (original code from the R CRAN package quantmod function getSymbols.FRED)
#' @references
#' \cite{Blame of R CRAN package quantmod function getSymbols and getSymbols.FRED
#' \url{https://github.com/joshuaulrich/quantmod/blame/master/R/getSymbols.R}
#' }
#' @references
#' \cite{Replicability, Real-Time Data, and the Science of Economic Research: FRED, ALFRED, and VDC, Richard G. Anderson, FEDERAL RESERVE BANK OF ST. LOUIS REVIEW JANUARY/FEBRUARY 2006, page 87-88
#' \url{https://files.stlouisfed.org/files/htdocs/publications/review/06/01/Anderson.pdf}
#' }
#'
#' @examples
#' \dontrun{
#'
#' library(quantmod)
#'
#' # Smoothed U.S. Recession Probabilities (RECPROUSM156N)
#' # Source: Piger, Jeremy Max, Chauvet, Marcelle
#' # https://fred.stlouisfed.org/data/RECPROUSM156N.txt
#' #
#' getSymbols("RECPROUSM156N", src =   "FRED")
#' [1] "RECPROUSM156N"
#'
#' getSymbols("RECPROUSM156N", src = "ALFRED", look.back = 4)
#'
#' Read 2002 items
#' Beginning Vintage: . . . 2012-09-04
#' Beginning Vintage: . . . 2013-09-03
#' Beginning Vintage: . . . 2014-09-01
#' Beginning Vintage: . . . 2015-09-01
#' Beginning Vintage: . . . 2016-09-01
#' Beginning Vintage: . . . 2017-09-01
#' Beginning Vintage: . . . 2018-09-03
#' Beginning Vintage: . . . 2019-10-01
#' Beginning Vintage: . . . 2020-10-01
#' [1] "RECPROUSM156N.vin"
#'
#' # Note, the distance between the graphic date and the 'Last Updated' date
#' # is about two(2) months, so one may shift the graphic to the right
#' # by two(2) months to get the real story.
#' #
#' # See: 'Date Range' and 'Last Updated' in https://fred.stlouisfed.org/data/RECPROUSM156N.txt
#' # See: getVintages("RECPROUSM156N")
#' #
#' # rough way to get the real story based on 'Date Range' and 'Last Updated'
#' index(RECPROUSM156N) <- index(RECPROUSM156N) + 61
#' index(RECPROUSM156N.vin) <- index(RECPROUSM156N.vin) + 61
#' dygraphs::dygraph(merge(RECPROUSM156N, RECPROUSM156N.vin, join = "inner"))
#'
#' # if too much time in periods exists between the
#' # tail date of  the 'Date Range' and 'Last Updated' date,
#' # then increase look.back from three(3) (default) to four(4) or greater.
#' getSymbols("RECPROUSM156N", src = "ALFRED", look.back = 4)
#'
#' # for debugging, include the vintages
#' getSymbols("RECPROUSM156N", src = "ALFRED", look.back = 4, datasheet = T)
#' View(data.frame(xtsAttributes(RECPROUSM156N.vin)$datasheet))
#'
#' # prepend (include) the data of the very first vintage in the head of the data
#' getSymbols("RECPROUSM156N", src = "ALFRED", look.back = 4, fullOldestVintageData = T)
#'
#' # Use R CRAN package doParallel to query simultaneously
#' getSymbols("RECPROUSM156N", src = "ALFRED", look.back = 4, allowParallel = T)
#'
#' # quarterly
#' # very large (that was back-loaded from 1991)
#' # Gross Domestic Product (GDP)
#' getSymbols("GDP", src = "ALFRED")
#' # noticably faster
#' getSymbols("GDP", src = "ALFRED", allowParallel = T)
#'
#' # weekly
#' # Weekly, Ending Friday (reported during the following Wednesday)
#' # Chicago Fed National Financial Conditions Index (NFCI)
#' getSymbols("NFCI", src = "ALFRED")
#'
#' # daily
#' # Effective Federal Funds Rate (EFFR)
#' getSymbols("EFFR", src = "ALFRED")
#' # better
#' getSymbols("EFFR", src = "ALFRED", vintages.per.query = 192)
#'
#' # the user does not want to query upon vintages before vintage 'Last Updated' date of "2020-01-01"
#' getSymbols("EFFR", src = "ALFRED", earlLastUpdDate = "2020-01-01")
#' #better (just recent data)
#' getSymbols("EFFR", src = "ALFRED", earlLastUpdDate = Sys.Date() - 35)
#'
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom foreach foreach `%do%` `%dopar%`
#' @importFrom doParallel registerDoParallel  stopImplicitCluster
#' @importFrom utils read.csv
#' @importFrom methods hasArg
#' @importFrom curl curl
#' @importFrom zoo as.Date as.yearmon as.yearqtr
#' @importFrom quantmod importDefaults getSymbols
getSymbols.ALFRED <- function(Symbols,
                              env,
                              return.class = "xts",
                              earlLastUpdDate,
                              look.back = 3,
                              vintages.per.query = 12,
                              fullOldestVintageData = F,
                              datasheet = F,
                              allowParallel = F,
                              ...) {
tryCatchLog::tryCatchLog({

  # if not done elsewhere
  #correct for TZ
  oldtz <- Sys.getenv("TZ")
  if(oldtz!="UTC") {
    Sys.setenv(TZ="UTC")
  }
  #
  assign("oldtz", oldtz, envir = environment())

  quantmod::importDefaults("getSymbols.ALFRED")
  this.env <- environment()
  for (var in names(list(...))) {
    assign(var, list(...)[[var]], this.env)
  }
  if (!methods::hasArg("verbose"))
    verbose <- FALSE
  if (!methods::hasArg("auto.assign"))
    auto.assign <- TRUE

  ALFRED.URL <- "https://alfred.stlouisfed.org/graph/alfredgraph.csv"
  returnSym <- Symbols
  noDataSym <- NULL
  for (i in seq_along(Symbols)) {
    if (verbose)
      cat("downloading ", Symbols[[i]], ".....\n\n")
    # Later I may want to change to _VIN
    # update (1 of 2 places)
    returnSym <- returnSym[returnSym %in% Symbols] <- paste0(Symbols[[i]], ".vin")
    test <- try({

      AllLastUpdatedDates <- getVintages(Symbols[[i]])
      #
      # just subtracts off some 'Last Updated' dates from the results of calling the function getVintages
      if(!is.null(earlLastUpdDate)) {
        AllLastUpdatedDates <- zoo::as.Date(AllLastUpdatedDates)[zoo::as.Date(earlLastUpdDate) <= zoo::as.Date(AllLastUpdatedDates)]
      }

      if(vintages.per.query == "Max") {
        vintages.per.query <- length(AllLastUpdatedDates)
      }

      # It has deep history.  Others (may) have shallow history.
      OldestVintageDate <- if(length(AllLastUpdatedDates)) { AllLastUpdatedDates[1] } else { character() }

      # to help determine how far to look back (minimze CPU)
      # from package xts
      # e.g. quarterly data e.g. GDP periodicity is "quarter" but re-reported each "month"
      # Often <- periodicity(as.Date(AllLastUpdatedDates))$label
      #
      # peek at current data to get an idea of the periodicity: "quarter", "month", "week", "day"
      Often <- periodicity(index(quantmod::getSymbols(Symbols[[i]], src = "FRED", auto.assign = FALSE)))$label

      # ALFRED limits 'vintages.per.query' (default 12) (groups of 'sheets of 'vintages.per.query' vintages')
      # Split a vector into chunks in R
      # https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r
      #
      SplittedLastUpdatedDates <- split(AllLastUpdatedDates, ceiling(seq_along(AllLastUpdatedDates)/vintages.per.query))

      FR <- xts()

      if(allowParallel) doParallel::registerDoParallel(cores = length(SplittedLastUpdatedDates))

      # something similar to what package caret function nominalTrainWorkflow does
      `%op%` <- if(allowParallel) { foreach::`%dopar%` } else { foreach::`%do%` }
      foreach::foreach (LastUpdatedDates = SplittedLastUpdatedDates, .packages = "xts") %op% {

        LengthOfLastUpdatedDates <- length(LastUpdatedDates)

        cat(paste0("Processing Vintages: . . . ", LastUpdatedDates[1], " . . . ", LastUpdatedDates[LengthOfLastUpdatedDates]), "\n")

        # vintages
        URL <- paste(ALFRED.URL, "?id=",           paste0(rep(Symbols[[i]], LengthOfLastUpdatedDates), collapse = ","), sep = "")
        # vintages last updated dates (validation)
        URL <- paste(       URL, "&vintage_date=", paste0(LastUpdatedDates,                            collapse = ","), sep = "")

        # The time distance between the Measurement interval and Validity interval is
        # not greater than two(three) periods ago (assuming/allowing one revision every so often)
        # so tell the server (and save CPU) to "not try and get past dates that are too old")
        #
        # How to subtract months from a date in R?
        # https://stackoverflow.com/questions/5225823/how-to-subtract-months-from-a-date-in-r/5226089

        # OCT 2020
        # ACTUALLY cosd is the DAY AFTER cosd AND THEN cosd is in the 'start period'
        # Affects cosd in  "quarter" and "month"

        # month
        # OCT 2020
        # https://alfred.stlouisfed.org/graph/alfredgraph.csv?id=RECPROUSM156N&vintage_date=2012-09-04&cosd=2012-06-04&coed=2012-09-04
        # . . .
        # 2012-05-01,0.40
        # 2012-06-01,1.02
        # TOO MUCH DATA - IF NO DATA FOUND, THEN RETURN EVERYTHING
        #
        # https://alfred.stlouisfed.org/graph/alfredgraph.csv?id=RECPROUSM156N&vintage_date=2012-09-04&cosd=2012-06-01&coed=2012-09-04
        # DATE,RECPROUSM156N_20120904
        # 2012-06-01,1.02
        # PERFECT

        # https://alfred.stlouisfed.org/graph/alfredgraph.csv?id=RECPROUSM156N&cosd=2012-01-04&coed=2012-09-04&vintage_date=2012-09-04
        # still the January period
        # https://alfred.stlouisfed.org/graph/alfredgraph.csv?id=RECPROUSM156N&cosd=2012-01-30&coed=2012-09-04&vintage_date=2012-09-04
        # next month period
        # https://alfred.stlouisfed.org/graph/alfredgraph.csv?id=RECPROUSM156N&cosd=2011-12-31&coed=2012-09-04&vintage_date=2012-09-04
        # https://alfred.stlouisfed.org/graph/alfredgraph.csv?id=RECPROUSM156N&cosd=2012-01-31&coed=2012-09-04&vintage_date=2012-09-04
        #

        # quarter
        # OCT 2020
        # perfect 4 quarters per year (and GDP has irregular co-dates)
        # https://alfred.stlouisfed.org/graph/alfredgraph.csv?id=GDP&cosd=2017-12-30&coed=2018-10-01&vintage_date=2018-10-01
        # https://alfred.stlouisfed.org/graph/alfredgraph.csv?id=GDP&cosd=2017-12-31&coed=2018-10-01&vintage_date=2018-10-01

        # weekly
        # Weekly, Ending Friday (reported during the following Wednesday)
        # https://alfred.stlouisfed.org/graph/alfredgraph.csv?id=NFCI&cosd=2020-09-26&coed=2020-10-02&vintage_date=2020-10-02
        # TOO MUCH DATA
        # FRI(+1 DAY)_TO_FRI # week - unlike "quarter" and "month", week can have different 'end of period' day-of-week
        # most of the time, it is consistenly reported weekly
        # https://alfred.stlouisfed.org/graph/alfredgraph.csv?id=NFCI&cosd=2020-09-25&coed=2020-10-02&vintage_date=2020-10-02

        # daily
        # daily (see look.back is increased to at least four(4)
        # this is reported during the next day (and does not report during weekends)
        # cosd is the DAY without modification
        # Monday reports the previous Friday
        # https://alfred.stlouisfed.org/graph/alfredgraph.csv?id=EFFR&cosd=2020-10-02&coed=2020-10-05&vintage_date=2020-10-05
        # Tuesday reports  yesterday Monday
        # https://alfred.stlouisfed.org/graph/alfredgraph.csv?id=EFFR&cosd=2020-10-03&coed=2020-10-06&vintage_date=2020-10-06


        if(Often == "day") {
          # need enough look.back, such that, Tuesday and after a three(3) day holiday weekend can see behind to the previous Friday.
          look.back <- max(4, look.back)
        }

        # do not look back more than three periods ago.  This limits server CPU and disk I/O.
        CoStartDates <- sapply(LastUpdatedDates, function(x) {as.character(seq(as.Date(x), length = 2, by = paste0("-", as.character(look.back), " ", Often, "s")))[2]})

        # exception to the "do not look back too far" is the oldest Vintage
        if(fullOldestVintageData && OldestVintageDate == names(CoStartDates[1])) {
          # part i of 2
          CoStartDates[1] <- "1776-07-04"
        }

        URL <- paste(URL, "&cosd=", paste0(CoStartDates, collapse = ","), sep = "")

        # Replicability, Real-Time Data, and the Science of Economic Research: FRED, ALFRED, and VDC
        # Richard G. Anderson
        # FEDERAL RESERVE BANK OF ST. LOUIS REVIEW JANUARY/FEBRUARY 2006
        # page 87-88
        # https://files.stlouisfed.org/files/htdocs/publications/review/06/01/Anderson.pdf
        #
        # Measurement interval <= Validity interval
        # so tell the server (and save CPU) to "not try and get future dates")
        CoEndDates   <- LastUpdatedDates

        URL <- paste(URL, "&coed=", paste0(CoEndDates, collapse = ","), sep = "")

        if (verbose)
          writeLines(URL)
        # go for it
        fr <- utils::read.csv(curl::curl(URL), na.string = ".")
        ColnamesFR <- colnames(fr)

        if (verbose)
          cat(paste0("done downloading\n"))
        fr <- xts(as.matrix(fr[, -1]), as.Date(fr[, 1], origin = "1970-01-01"),
                  src = "ALFRED", updated = Sys.time())

        # part ii of 2
        # Reactionary protection against: TOO MUCH DATA
        # IF NO DATA FOUND, then it RETURNS EVERYTHING (TOO MUCH DATA)

        trueCoStartDate <- CoStartDates[1]
        # (FROM ABOVE) cosd is the DAY AFTER cosd AND THEN cosd is in the 'start period'
        if(Often == "quarter") {
          trueCoStartDate <- zoo::as.Date(zoo::as.yearqtr(zoo::as.Date(CoStartDates[1]) + 1))
        }
        if(Often == "month") {
          trueCoStartDate <- zoo::as.Date(zoo::as.yearmon(zoo::as.Date(CoStartDates[1]) + 1))
        }
        if(Often == "week") {
          trueCoStartDate <- zoo::as.Date(CoStartDates[1]) + 1
          # since weeklies are reported often weekly, then look.back == 3' should (hopefully) cover this date range
          # CODE MAY BE FRAGILE HERE
          # stop("Truncation test of . . . Often = \"week\" is not yet implemented.")
        }
        if(Often == "day") {
          # cosd is the DAY without modification
          trueCoStartDate <- zoo::as.Date(CoStartDates[1]) + 0
          # CODE MAY BE FRAGILE HERE
          # stop("Truncateion test of . . . Often = \"day\" is not yet implemented.")
        }

        frNROW.orig <- NROW(fr)
        fr <- fr[paste(trueCoStartDate, "::", sep = ""),]
        if(frNROW.orig != NROW(fr)) {
          writeLines(paste(frNROW.orig - NROW(fr), " old records truncated.  \n  Possibly zero(0) records returned, so possibly TOO MUCH DATA is returned.  \n    Consider INCREASING PARAMETER look.back to be GREATER THAN ", look.back,".", sep = ""))
        }

        # NOTE: return.class must be able to handle mult-dimensional
        # this removes column names
        dim(fr) <- c(NROW(fr), NCOL(fr))

        # Symbol name is already in the column + _ YYMMDD
        colnames(fr) <- as.character(toupper(ColnamesFR[-1]))

        # S3 call to cbind.xts: FR left of fr will set wrong XtsAttributes
        tclass(FR) <- tclass(fr); tformat(FR) <- tformat(fr); tzone(FR) <- tzone(fr)

        FR <- fr
        xtsAttributes(FR) <- xtsAttributes(fr)

        FR

      } -> ListFR
      if(allowParallel) doParallel::stopImplicitCluster()

      FR <- do.call(cbind, ListFR)

      fr <- FR
      xtsAttributes(fr)$oldestvintage <- OldestVintageDate
      # keep FR.  I later attach the datasheet to xtsAttributes
      FR <- fr

      # How to implement coalesce efficiently in R
      # https://stackoverflow.com/questions/19253820/how-to-implement-coalesce-efficiently-in-r

      # need to
      # Rotate a Matrix in R
      # https://stackoverflow.com/questions/16496210/rotate-a-matrix-in-r
      #
      rotate <- function(x) t(apply(x, 2, rev))

      # need to pull data the new vintage data to the left (into to the oldest vintage column NA entries),
      # This ruins OTHER columns (non-oldest-vintage)
      # note "na.locf" is from package xts
      NewCoreData <- rotate(na.locf(rotate(rotate(rotate((fr))))))
      # redefine (seems must be the same size)
      coredata(fr) <- NewCoreData
      # need to just keep the oldest vintage column
      fr <- fr[,1]
      # need to rename (the ONE column)
      colnames(fr)[1] <- Symbols[[i]]

      fr <- quantmod___convert.time.series(fr = fr, return.class = return.class)
      Symbols[[i]] <- toupper(gsub("\\^", "", Symbols[[i]]))

      # update (2 of 2 places)
      Symbols[[i]]    <- paste0(Symbols[[i]], ".vin")
      colnames(fr)[1] <- Symbols[[i]]

      # debugging
      if(datasheet == T) {
        xtsAttributes(fr)$datasheet <- list(FR)
      }

      if (auto.assign)
        assign(Symbols[[i]], fr, env)
    }, silent = TRUE)

    Sys.setenv(TZ=oldtz)

    if (inherits(test, "try-error")) {
      msg <- paste0("Unable to import ", dQuote(returnSym[[i]]),
                    ".\n", attr(test, "condition")$message)
      if (hasArg(".has1sym.") && match.call(expand.dots = TRUE)$.has1sym.) {
        stop(msg)
      }
      warning(msg, call. = FALSE, immediate. = TRUE)
      noDataSym <- c(noDataSym, returnSym[[i]])
    }
  }
  if (auto.assign)
    return(setdiff(returnSym, noDataSym))
  return(fr)
})}
