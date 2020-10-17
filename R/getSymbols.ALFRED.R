
#' Download Federal Reserve Economic Data - ALFRED(R)
#'
#' R access to the latest observation(s) of the vintages of thousands of data series accessible via the St. Louis Federal Reserve Bank's ALFRED (Archival FRED:  Federal Reserve Bank of St. Louis's _Archiva_L  _Federal _Reserve _Economic _Data) system; collects and displays data from the ALFRED vintages that (as seen by a public user during a zone of time at FRED), 'per observation date', are the 1st appearance of an observation date (and its datum).
#'
#' Downloads Symbols to specified environment (variable env) from 'research.stlouisfed.org'. This method is not to be called directly, instead a call to R CRAN packages quantmod function getSymbols(Symbols,src='ALFRED') will in turn call this method.
#'
#' The St. Louis Federal Reserve Bank's FRED allows a specific data series to be revised(overwritten) called vintages with different data while and displaying new objservation dates and re-using the same observation dates of the same observations from a 'Date Range'.  The series author very often replaces the each data series with a new one. This replacement activity is data obscurity.  The past data series of FRED is not easily accessible. The past data series are accessible through ALFRED. The current data series of FRED and the past data series (that are no longer seen in FRED) are called vintages.  The original non-rewritten data is called the 1st vintage.  The (first) revision is called the 2nd vintage.  The (second) next revision is called the 3rd vintage and so-on.  The last revision is displayed at FRED. This function collects and displays data from the ALFRED vintages that 'per observation date', are the 1st appearance of an observation date (and its datum).
#'
#' The functions is a tool in the process of examining a FRED data series of interest and identifying FRED data series that 'per observation' date and its datam, have much change compared to the next vintage's (same) 'per observation' date and its (potentially different) datum.  The process is looking for FRED data series that have vintages with highly volatile data, relative to the (same) 'per observation' date and its (potentially different) datum.
#'
#' @param Symbols a character vector specifying the names of each symbol to be loaded (from R CRAN package quantmod function getSymbols)
#' @param env where to create objects. (.GlobalEnv) (from R CRAN package quantmod function getSymbols)
#' @param return.class class of returned object (from R CRAN package quantmod function getSymbols)
#' @param EarliestLastUpdDate character or Date.  Earliest date that is before or 'at' the vintage 'Last Updated' date in the past that a user may wish to query upon. Default is NULL (no restriction).  This is useful in the situation when the user already owns prior data, and just wants just some recent data.  Internally, this just subtracts off some 'Last Updated' dates from the results of calling the function getVintages.
#' @param VintagesPerQuery number of vintages per HTTPS GET. A.k.a the number of vintages per sheet.   Default is 12.  Common maximum is 12. Value can be "Max". Practical experience has performed with 192.  The maximum may be different during different with not-a-known reason.  This parameter exists to enhance performance by limiting the number of trips to the server.  This parameter is sometimes (but not often) better than the parameter allowParallel. On many occasions  when using this parameter with values greater than 12, the requested data is missing from the returned data set.
#' @param LookBack how deep in periods to look back for the latest observation in all of the non-oldest vintages.  Meant to use with datasets with a wide range of time between the Measurement interval and the Validity interval.  From the 'Last Updated' date try to peek back in time to the 1st vintage with a published tail 'Date Range' date that is within variable 'LookBack' periods. If the periodicy is "day" and, just after a three(3) day holiday weekend, to reach back from a Tuesday to a Friday, parameter LookBack is increased to a minimum value of 4.  Default is 3.  Increase this value if much time exists between the tail date of 'Date Range' and the 'Last Updated' date: meaning zero(0) observations exist in the LookBack period.  The R CRAN package xts function periodicity determines the period of time.  This function is meant to minimize server-side CPU and disk I/O.
#' @param FullOldestVintageData if TRUE, then also return the oldest vintage data and keep(prepend) its data.  Default is FALSE. Useful when 'as much data as possible' is important.
#' @param DataSheet if TRUE, then also return all of the vintages in an xts attribute 'DataSheet'. Default is FALSE.  Useful for debugging.  Useful as a tool of doing more (future) coding or user-end research.
#' @param allowParallel if TRUE, then collect groups of 'sheets of VintagesPerQuery vintages' in parallel.  Default is FALSE.  (Improved) performance will vary: this is more useful on (more data points) weekly data or daily data. Because this is a server side activity, the number of parallel processes does NOT depend on the local machine CPUs.
#' @param MaxParallel if allowParallel is TRUE, then set the maximum number of parallel processes. Default is NULL (no limit).  If this parameter is NULL, then the approximate maximum number of parallel processes is 'unique(ceiling(seq_along(getVintages(SYMBOL)/VintagesPerQuery)))' where the vector from getVintages(SYMBOL) may be reduced by limiting data using EarliestLastUpdDate. Good choices of this parameter may depend on, the amount of the client host hardware CPU and memory.
#' @param ... additional parameters
#'
#' @author Andre Mikulec   (adapted from the original code)
#' @author Jeffrey A. Ryan (original code from the R CRAN package quantmod function getSymbols.FRED)
#' @references
#' \cite{Blame of the code of the R CRAN package quantmod function getSymbols and getSymbols.FRED
#' \url{https://github.com/joshuaulrich/quantmod/blame/master/R/getSymbols.R}
#' }
#' @references
#'\cite{ALFRED: Capturing data as it happens, Katrina Stierholz, Director of Library and Information Services, Federal Reserve Bank of St. Louis,
#'\url{https://alfred.stlouisfed.org/docs/alfred_capturing_data.pdf}
#'}
#' @references
#' \cite{Richard G. Anderson, "Replicability, Real-Time Data, and the Science of Economic Research: FRED, ALFRED, and VDC," Federal Reserve Bank of St. Louis Review, January/February 2006, pp. 81-94.
#' \url{https://doi.org/10.20955/r.88.81-94}
#' \url{https://files.stlouisfed.org/files/htdocs/publications/review/06/01/Anderson.pdf}
#' }
#' @examples
#' \dontrun{
#'
#' # Smoothed U.S. Recession Probabilities (RECPROUSM156N)
#' # Source: Piger, Jeremy Max, Chauvet, Marcelle
#' # https://fred.stlouisfed.org/series/RECPROUSM156N
#' # and
#' # https://fred.stlouisfed.org/data/RECPROUSM156N.txt
#' #
#' getSymbols("RECPROUSM156N", src =   "FRED")
#' [1] "RECPROUSM156N"
#'
#' getSymbols("RECPROUSM156N", src = "ALFRED", LookBack = 4)
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
#' # then increase LookBack from three(3) (default) to four(4) or greater.
#' getSymbols("RECPROUSM156N", src = "ALFRED", LookBack = 4)
#'
#' # for debugging, or further work one may include the vintages
#' getSymbols("RECPROUSM156N", src = "ALFRED", LookBack = 4, DataSheet = T)
#' # note: the user-chosen 'viewer' may have a limit on the number of columns displayed
#' View(data.frame(xtsAttributes(RECPROUSM156N.vin)$DataSheet))
#' # see the names of all columns
#' #   The column names format are the same names and formate as returned
#' #   by ALFRED (and unmodified) '(old)FRED series identifier'_YYYYMMDD.
#' #   YYYYMMDD is the 'Last Updated' date;
#' #     YYYY is the year and
#' #     MM is the month in number values of 01 through 12 representing January through December.
#' #     DD is the daty of the month from 01 through the last day of that month.
#' colnames(data.frame(xtsAttributes(RECPROUSM156N.vin)$DataSheet))
#'
#' # From (above) https://fred.stlouisfed.org/data/RECPROUSM156N.txt
#' #    one knows that Frequency is 'Monthly'.
#' # To get  all of the data (in all of the vintages), set the
#' #   LookBack to be a 'high value' e.g. perhaps, 100 years (1200: 12 months x 100 years)
#' #   (In this specific FRED series case, observation dates start in 1967)
#' getSymbols("RECPROUSM156N", src = "ALFRED", LookBack = 1200, DataSheet = T)
#' View(data.frame(xtsAttributes(RECPROUSM156N.vin)$DataSheet))
#'
#' # prepend (include) the data of the very first vintage in the head of the data
#' getSymbols("RECPROUSM156N", src = "ALFRED", LookBack = 4, FullOldestVintageData = T)
#'
#' # use R CRAN package doParallel to query simultaneously
#' getSymbols("RECPROUSM156N", src = "ALFRED", LookBack = 4, allowParallel = T)
#'
#' # quarterly
#' # ALFRED was still in development in 2006;
#' #   moreover, not all FRED data series were being placed into ALFRED.
#' # Some FRED data series were/is popular enough and important enough
#' #   to be worth the effort to back-load its data into ALFRED.
#' # Gross Domestic Product (GDP)
#' #   is in ALFRED with 'Last Updated' dates that start in the year of 1991.
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
#' # sometimes better
#' # In the case of  using a large 'VintagesPerQuery',
#' #   very often much requested data is missing from the returned data.
#' getSymbols("EFFR", src = "ALFRED", VintagesPerQuery = 192)
#' # often better
#' getSymbols("EFFR", src = "ALFRED", allowParallel = T, MaxParallel = 8)
#'
#' # the user does not want to query upon vintages before vintage at the 'Last Updated' date of "2020-01-01"
#' getSymbols("EFFR", src = "ALFRED", EarliestLastUpdDate = "2020-01-01")
#' # better (just recent data)
#' getSymbols("EFFR", src = "ALFRED", EarliestLastUpdDate = Sys.Date() - 35)
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
                              EarliestLastUpdDate = NULL,
                              LookBack = 3,
                              VintagesPerQuery = 12,
                              FullOldestVintageData = F,
                              DataSheet = F,
                              allowParallel = F,
                              MaxParallel = NULL,
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
      if(!is.null(EarliestLastUpdDate)) {
        AllLastUpdatedDates <- zoo::as.Date(AllLastUpdatedDates)[zoo::as.Date(EarliestLastUpdDate) <= zoo::as.Date(AllLastUpdatedDates)]
      }

      if(VintagesPerQuery == "Max") {
        VintagesPerQuery <- length(AllLastUpdatedDates)
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
      # note, could read the Frequency here: https://fred.stlouisfed.org/data/GDP.txt

      # ALFRED limits 'VintagesPerQuery' (default 12) (groups of 'sheets of 'VintagesPerQuery' vintages')
      # Split a vector into chunks in R
      # https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r
      #
      SplittedLastUpdatedDates <- split(AllLastUpdatedDates, ceiling(seq_along(AllLastUpdatedDates)/VintagesPerQuery))

      FR <- xts()

      if(allowParallel) {
        if(!is.null(MaxParallel)) {
          MaxDoParallelCores <- MaxParallel
        } else {
          MaxDoParallelCores <- length(SplittedLastUpdatedDates)
        }
        doParallel::registerDoParallel(cores = MaxDoParallelCores)
      }

      # something similar to what package caret function nominalTrainWorkflow does
      `%op%` <- if(allowParallel) { foreach::`%dopar%` } else { foreach::`%do%` }
      foreach::foreach (LastUpdatedDates = SplittedLastUpdatedDates, .packages = "xts") %op% {

        LengthOfLastUpdatedDates <- length(LastUpdatedDates)

        cat(paste0("Processing vintages: . . . ", LastUpdatedDates[1], " . . . ", LastUpdatedDates[LengthOfLastUpdatedDates]), "\n")

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
        # daily (see LookBack is increased to at least four(4)
        # this is reported during the next day (and does not report during weekends)
        # cosd is the DAY without modification
        # Monday reports the previous Friday
        # https://alfred.stlouisfed.org/graph/alfredgraph.csv?id=EFFR&cosd=2020-10-02&coed=2020-10-05&vintage_date=2020-10-05
        # Tuesday reports  yesterday Monday
        # https://alfred.stlouisfed.org/graph/alfredgraph.csv?id=EFFR&cosd=2020-10-03&coed=2020-10-06&vintage_date=2020-10-06


        if(Often == "day") {
          # need enough LookBack, such that, Tuesday and after a three(3) day holiday weekend can see behind to the previous Friday.
          LookBack <- max(4, LookBack)
        }

        # do not look back more than three periods ago.  This limits server CPU and disk I/O.
        CoStartDates <- sapply(LastUpdatedDates, function(x) {as.character(seq(as.Date(x), length = 2, by = paste0("-", as.character(LookBack), " ", Often, "s")))[2]})

        # exception to the "do not look back too far" is the oldest Vintage
        if(FullOldestVintageData && OldestVintageDate == names(CoStartDates[1])) {
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
          # since weeklies are reported often weekly, then LookBack == 3' should (hopefully) cover this date range
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
          writeLines(paste(frNROW.orig - NROW(fr), " old records truncated.  \n  Possibly zero(0) records returned, so possibly TOO MUCH DATA is returned.  \n    Consider INCREASING PARAMETER LookBack to be GREATER THAN ", LookBack,".", sep = ""))
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
      xtsAttributes(fr)$OldestVintage <- OldestVintageDate
      # keep FR.  I later attach the DataSheet to xtsAttributes
      FR <- fr

      # # How to implement coalesce efficiently in R
      # # https://stackoverflow.com/questions/19253820/how-to-implement-coalesce-efficiently-in-r
      #
      # # need to
      # # Rotate a Matrix in R
      # # https://stackoverflow.com/questions/16496210/rotate-a-matrix-in-r
      # #
      # rotate <- function(x) t(apply(x, 2, rev))
      #
      # # need to pull data the new vintage data to the left (into to the oldest vintage column NA entries),
      # # This ruins OTHER columns (non-oldest-vintage)
      # # note "na.locf" is from package xts
      # NewCoreData <- rotate(na.locf(rotate(rotate(rotate((fr))))))

      # debugged by using "GDP" and EarliestLastUpdDate "Sys.Date() - 300" during OCT 17 2020
      #
      # Finding the 'first available datam per specific date' of all vintages
      #
      FrMatrix <- as.matrix(fr)
      #
      # now reversed: rows are now vintages, columns are now observation dates
      FrMatrixTransposed <- t(FrMatrix)
      #
      # of the row values (keep) highest value rows(latest in time) (vintage 'Last Updated')
      #   are at the right (no change)
      # of the column values, highest value columns(latest in time) (observation dates)
      #   are at top (change)
      FrMatrixTransposedUpsideDown <- FrMatrixTransposed[rev(seq_along(rownames(FrMatrixTransposed))), ]
      #
      # Browse[2]> FrMatrixTransposedUpsideDown
      #              2019-04-01 2019-07-01 2019-10-01 2020-01-01 2020-04-01
      # GDP_20200930         NA         NA   21747.39   21561.14   19520.11
      # GDP_20200827         NA         NA   21747.39   21561.14   19486.51
      # GDP_20200730         NA         NA   21747.39   21561.14   19408.76
      # GDP_20200625         NA   21542.54   21729.12   21539.69         NA
      # GDP_20200528         NA   21542.54   21729.12   21534.91         NA
      # GDP_20200429         NA   21542.54   21729.12   21537.94         NA
      # GDP_20200326   21340.27   21542.54   21729.12         NA         NA
      # GDP_20200227   21340.27   21542.54   21726.78         NA         NA
      # GDP_20200130   21340.27   21542.54   21734.27         NA         NA
      #
      # # of the 'first available datam per specific date' of all vintages,
      # #   pull its datum down into the last row(na.locf)
      # # discard all other rows (they are now garbage)
      # # of the last (only) row, discard the meaningless 'rowname' (now garage)
      # #   by redefining as a one-row-matrix with no rowname (matrix)
      # # format the data into an acceptable form that can input into coredata (t)
      # NewCoreData <- t(matrix(last(na.locf(FrMatrixTransposedUpsideDown)), nrow = 1, dimnames = list(NULL, colnames(FrMatrixTransposedUpsideDown))))
      #
      # of the 'first available datam per specific date' of all vintages,
      #   pull its datum down into a single vector
      #   and format that data as input into package xts function as.xts
      NewCoreData <- matrix(apply(FrMatrixTransposedUpsideDown, MARGIN = 2, function(x) last(na.omit(x))), dimnames = list(colnames(FrMatrixTransposedUpsideDown), NULL))
      # note list rownames is just for display here (just below).
      #      Rownames will be later discarded by '<- index(fr)'
      #
      # Browse[2]> NewCoreData
      # [,1]
      # 2019-04-01 21340.27
      # 2019-07-01 21542.54
      # 2019-10-01 21734.27
      # 2020-01-01 21537.94
      # 2020-04-01 19408.76

      # redefine (seems must be the same size)
      # coredata(fr) <- NewCoreData
      #
      # not the same size
      frNew <- as.xts(NewCoreData)
      index(frNew) <- index(fr)
      xtsAttributes(frNew) <- xtsAttributes(fr)
      fr <- frNew

      # need to just keep the oldest vintage column (useless op if ran 'not the same size')
      fr <- fr[,1]
      # need to rename (the ONE column)
      colnames(fr)[1] <- Symbols[[i]]

      fr <- quantmod___convert.time.series(fr = fr, return.class = return.class)
      Symbols[[i]] <- toupper(gsub("\\^", "", Symbols[[i]]))

      # update (2 of 2 places)
      Symbols[[i]]    <- paste0(Symbols[[i]], ".vin")
      colnames(fr)[1] <- Symbols[[i]]

      # debugging
      if(DataSheet == T) {
        xtsAttributes(fr)$DataSheet <- FR
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
