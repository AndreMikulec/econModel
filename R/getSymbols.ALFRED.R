




#' S&P 500 Stock Market Data from Yale University
#'
#' @description
#' Robert Shillers S&P 500 Stock Market Data from his Excel spreadsheet as a R CRAN package quantmod function getSymbols source (src).
#'
#' @param Symbols  A character vector specifying the names of each symbol to be loaded.
#' Possible Symbols are the following:
#' "YaleUStockMarketData" (means get all of the columns) xor
#' one specific column name. e.g. "CAPE"
#' @param env where to create objects. (.GlobalEnv)
#' @param return.class desired class of returned object.
#' Can be xts, zoo, data.frame, or xts (default)
#' @param returnIndex one of "ObservationDate" (row element date) or "LastUpdatedDate" (dates of estimations of in cases such that estimations are done?). Default is ObservationDate".  Note, an 'observation date'(row element date) is  not the 'date of estimation'. The 'observation date' (typically) observes the beginning of the 'date range' (its period: ObservationDate + Frequency).  The LastUpdatedDate date, that is, the date of estimation, is after the the period has completed, that is after  ObservationDate + Frequency.
#' @param ... Dots passed.
#' @return A call to getSymbols.YaleU will load into the specified
#' environment one object for each \code{Symbol} specified,
#' with class defined by \code{return.class}.
#' @author Andre Mikulec (adapted original code to work with Yale University stock market data)
#' @references
#' \cite{Online Data of Robert Shiller
#' \url{http://www.econ.yale.edu/~shiller/data.htm}}
#' @references
#' \cite{Home Page of Robert J. Shiller - Yale University
#' \url{http://www.econ.yale.edu/~shiller/}}
#' @examples
#' \dontrun{
#' # common usage
#' getSymbols(c("CAPE", "Dividends", "Earnings"), src = "YaleU")
#'
#' # all columns in one xts object
#' getSymbols("YaleUstockMarketData", src = "YaleU")
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom quantmod importDefaults
#' @importFrom readxl read_xls
#' @importFrom DescTools AddMonths
#' @importFrom Hmisc yearDays
#' @importFrom zoo as.Date na.trim
#' @importFrom xts xts xtsAttributes `xtsAttributes<-`
#' @export
getSymbols.YaleU <- function(Symbols, env, return.class = "xts",
                             returnIndex = "ObservationDate",
                             ...) {
tryCatchLog::tryCatchLog({

  # if not done elsewhere
  #correct for TZ
  oldtz <- Sys.getenv("TZ")
  if(oldtz!="UTC") {
    Sys.setenv(TZ="UTC")
  }

  # determine temp folder
  ops <- options()
  if(!"econModel.getSymbols.YaleU.folder" %in% Names(ops)) {
    options(append(ops, list(econModel.getSymbols.YaleU.folder = paste0(normalizePath(tempdir(), winslash = "/"), "/", "econModel.getSymbols.YaleU.folder"))))
  }

  # verify/create temp folder
  if(!dir.exists(getOption("econModel.getSymbols.YaleU.folder"))) {
    dir.create(getOption("econModel.getSymbols.YaleU.folder"))
  }

  quantmod::importDefaults("getSymbols.YaleU")

  this.env <- environment()
  for (var in names(list(...))) {
    assign(var, list(...)[[var]], this.env)
  }
  if (!hasArg("verbose"))
    verbose <- FALSE
  if (!hasArg("auto.assign"))
    auto.assign <- TRUE

  # begin xls area
  YALEU.URL <- "http://www.econ.yale.edu/~shiller/data/ie_data.xls"
  DestFile  <- paste0(normalizePath(getOption("econModel.getSymbols.YaleU.folder"), winslash = "/"), "/", "ie_data.xls")
  if(!file.exists(DestFile)) {
    DirFilePath <- fetchInternet(YALEU.URL, Structure = "BinFile", DestFile = DestFile)
  }

  if (verbose) {
    cat("Reading disk file ", DestFile, ".....\n\n")
  }

  # Last verified: NOV 2020
  # He sometimes changes his columns (and this breaks)
  ColNames <- read.table(text="
  Date
  SP500Price
  Dividends
  Earnings
  ConsPriceInd
  DateFract
  LongIntRate
  RealPrice
  RealDividend
  RealTotRetPrice
  RealEarning
  RealTRScaledEarning
  CAPE
  Empty1
  TRCAPE
  Empty2
  ExcessCAPEYld
  MoGrossBndRets
  RealTotBndRets
  AnnNext10YrStockRealRet
  AnnNext10YrBondsRealRet
  AnnRealNext10YrExcessRets
  ", stringsAsFactors = FALSE)
  ColNames <- unlist(ColNames)

  # Reports somewhere in the time range of the middle (15th,16th, or 17th)
  # of the current month (SEE below)
  MonBeginDates <- seq.Date(from = zoo::as.Date("1871-01-01"), to = Sys.Date(), by = "1 month")
  # Overshot by one(1): Includes next months first day (that has not happened yet)

  # data.frame
  fr <- suppressWarnings(readxl::read_xls(path = DestFile, sheet = "Data",
                                          col_names = ColNames,
                                          col_types = rep("numeric", length(ColNames)), skip = 8L,
                                          n_max = length(MonBeginDates)))

  fr <- fr[, !colnames(fr) %in% c("Empty1", "Empty2")]

  # optimistic: but the last record
  # (current month 'not yet reported') may be all NAs
  fr <- zoo::na.trim( fr, sides = "right", is.na = "all")

  # FRED historical ... dates are the "first of the month"
  # that datum means "about that ENTIRE PREVIOUS MONTH"
  # KEEP
  Dates <- zoo::as.Date(paste0(as.character(as.integer(round(fr[["Date"]], 2) * 100)), "01"), format = "%Y%m%d")
  Dates <- DescTools::AddMonths(Dates, - 1L)

  # Date Fraction of Yale (DateFY)
  DatesFYYear          <- trunc(fr[["DateFract"]],0)
  DatesFYYearFraction  <- fr[["DateFract"]] - DatesFYYear
  #
  DatesFYYearFirstDate <- zoo::as.Date(paste0(DatesFYYear, "-01-01"))
  IntegerDateFYYearFirstDate <- as.integer(DatesFYYearFirstDate)
  #
  DatesFYNumbDaysInYear <- Hmisc::yearDays(DatesFYYearFirstDate)
  #
  # Stored as the number of days since the UNIX Epoch(UTC time)
  DatesFY <- IntegerDateFYYearFirstDate + DatesFYNumbDaysInYear * DatesFYYearFraction
  # Keep
  DatesFY <- zoo::as.Date(DatesFY)

  if(returnIndex == "ObservationDate") {
    Index <- Dates
    OtherDateIndexAttr <- list(LastUpdatedDate = DatesFY)
  }
  if(returnIndex == "LastUpdatedDate") {
    Index <- DatesFY
    OtherDateIndexAttr <- list(ObservationDate = Dates)
  }

  fr <- fr[, !colnames(fr) %in% c("Date", "DateFract")]

  if (verbose)
    cat("Done.\n\n")

  fr <- xts::xts(as.matrix(fr), Index,
                 src = "YaleU", updated = Sys.time())

  xts::xtsAttributes(fr) <- c(list(), xts::xtsAttributes(fr), OtherDateIndexAttr)

  fri <- fr # pass-throught on "YaleUstockMarketData"

  # decompose [if any] into [many] Symbol(s), then return
  for (i in seq_along(Symbols)) {

    if (verbose) {
      cat("Selecting ", Symbols[[i]], ".....\n\n")
    }
    # User only wants an individual column
    if(Symbols[[i]] != "YaleUstockMarketData") {
      fri <- fr[, Symbols[[i]]]
    }
    fri <- quantmod___convert.time.series(fr = fri, return.class = return.class)
    if (auto.assign)
      assign(Symbols[[i]], fri, env)
  }

  Sys.setenv(TZ=oldtz)

  if (auto.assign)
    return(Symbols)
  return(fri)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' American Association of Individual Investors (AAII) Weekly Sentiment Survey
#'
#' @description
#' Data represents what direction members feel the
#' stock market will be in next 6 months.
#'
#' The sentiment survey measures the percentage of individual investors who are
#' bullish, bearish, and neutral on the stock market short term;
#' individuals are polled from the AAII Web site on a weekly basis.
#' Only one vote per member is accepted in each weekly voting period.
#'
#' The AAII Sentiment Survey is conducted each week from Thursday 12:01 a.m. until Wednesday at 11:59 p.m. The latest published data seems to be delivered every Friday.
#'
#' Internally, this reads a (downloaded) Microsoft Excel spreadsheet
#' and collects just the one latest published item by scraping the
#' one new (the latest) observation from the AAII web site.
#'
#' @param Symbols  A character vector specifying the names of each symbol to be loaded.
#' Possible Symbols are the following:
#' "AAIISentimentSurveyData" (means get all of the columns) xor
#' one specific column name. e.g. "Bullish", "Neutral", or "Bearish", (or Others).
#' @param env where to create objects. (.GlobalEnv)
#' @param return.class desired class of returned object.
#' Can be xts, zoo, data.frame, or xts (default)
#' @param ... Dots passed.
#' @return A call to getSymbols.AAIISentiment will load into the specified
#' environment one object for each \code{Symbol} specified,
#' with class defined by \code{return.class}.
#' @author Andre Mikulec (adapted original code to work with AAII sentiment data)
#' @author Jeffrey A. Ryan
#' @references
#' \cite{AAII Investor Sentiment Survey
#' \url{https://www.aaii.com/sentimentsurvey}}
#' @references
#' \cite{Sentiment Survey Past Results
#' \url{https://www.aaii.com/sentimentsurvey/sent_results}}
#' @references
#' \cite{Charles Rotblut, Analyzing the AAII Sentiment Survey Without Hindsight, AAII Journal / June 2014
#' \url{https://www.aaii.com/journal/article/analyzing-the-aaii-sentiment-survey-without-hindsight}}
#' @references
#' \cite{Charles Rotblut, Is the AAII Sentiment Survey a Contrarian Indicator?, AAII Journal / June 2013
#' \url{https://www.aaii.com/journal/article/is-the-aaii-sentiment-survey-a-contrarian-indicator}}
#' @examples
#' \dontrun{
#' # common usage
#'getSymbols("BullBearSpread", src = "AAIISentiment")
#'
#' # The dates are as of Thursday each week.
#' getSymbols(c("Bullish", "Neutral", "Bearish"), src = "AAIISentiment")
#'
#' # all columns in one xts object
#' getSymbols("AAIISentimentSurveyData", src = "AAIISentiment")
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom quantmod importDefaults
#' @importFrom readxl read_xls
#' @importFrom htmltab htmltab
#' @importFrom DescTools Weekday
#' @importFrom zoo index as.Date na.trim
#' @importFrom xts xts xtsAttributes `xtsAttributes<-`
#' @export
getSymbols.AAIISentiment <- function(Symbols, env, return.class = "xts",
                                     ...) {
tryCatchLog::tryCatchLog({

  # if not done elsewhere
  #correct for TZ
  oldtz <- Sys.getenv("TZ")
  if(oldtz!="UTC") {
    Sys.setenv(TZ="UTC")
  }

  # determine temp folder
  ops <- options()
  if(!"econModel.getSymbols.AAIISentiment.folder" %in% Names(ops)) {
    options(append(ops, list(econModel.getSymbols.AAIISentiment.folder = paste0(normalizePath(tempdir(), winslash = "/"), "/", "econModel.getSymbols.AAIISentiment.folder"))))
  }

  # verify/create temp folder
  if(!dir.exists(getOption("econModel.getSymbols.AAIISentiment.folder"))) {
    dir.create(getOption("econModel.getSymbols.AAIISentiment.folder"))
  }

  quantmod::importDefaults("getSymbols.AAIISentiment")

  this.env <- environment()
  for (var in names(list(...))) {
    assign(var, list(...)[[var]], this.env)
  }
  if (!hasArg("verbose"))
    verbose <- FALSE
  if (!hasArg("auto.assign"))
    auto.assign <- TRUE

  # begin xls area
  AAIISENTIMENT.URL <- "https://www.aaii.com/files/surveys/sentiment.xls"
  DestFile  <- paste0(normalizePath(getOption("econModel.getSymbols.AAIISentiment.folder"), winslash = "/"), "/", "sentiment.xls")
  if(!file.exists(DestFile)) {
    DirFilePath <- fetchInternet(AAIISENTIMENT.URL, Structure = "BinFile", DestFile = DestFile)
  }

  if (verbose) {
    cat("Reading disk file ", DestFile, ".....\n\n")
  }

  #Changes of names of  columns (and this breaks)
  ColNames <- read.table(text="
Date
Bullish
Neutral
Bearish
Total
Bullish8WeekMovAve
BullBearSpread
BullishAve
BullishAvePlusStd
BullishAveMinusStd
SP500WeeklyHigh
SP500WeeklyLow
SP500WeeklyClose
", stringsAsFactors = FALSE)
  ColNames <- unlist(ColNames)

  # Reports somewhere in the time range of the middle (15th,16th, or 17th)
  # of the current month (SEE below)
  # THIS VARIABLE (IS CURRENLTY) *ONLY* USED TO DO SIZING
  BeginPeriodDates <- seq.Date(from = zoo::as.Date("1987-06-26"), to = Sys.Date(), by = "7 day")
  BeginPeriodDates <- BeginPeriodDates - 6L
  # Early survey weeks were missed
  BeginPeriodDates <- BeginPeriodDates[c(-2,-3)]
  # Begins Previous Saturday # Through Thursday(Observation: NOT USING)
  # Delivered to users (And Ending) during Friday(Published)

  # data.frame
  fr <- suppressWarnings(readxl::read_xls(path = DestFile, sheet = "SENTIMENT",
                                          col_names = ColNames,
                                          col_types = c("date", rep("numeric", length(ColNames) - 1L)), skip = 5L,
                                          n_max = length(BeginPeriodDates)))

  # optimistic: but the last record
  # (current month 'not yet reported') may be all NAs
  fr <- zoo::na.trim( fr, sides = "right", is.na = "all")

  # KEEP
  Dates <- zoo::as.Date(fr[["Date"]])

  Index <- Dates
  OtherDateIndexAttr <- list()

  fr <- fr[, !colnames(fr) %in% c("Date")]

  if (verbose)
    cat("Done.\n\n")

  fr <- xts::xts(as.matrix(fr), Index,
                 src = "AAIISentiment", updated = Sys.time())

  xts::xtsAttributes(fr) <- c(list(), xts::xtsAttributes(fr), OtherDateIndexAttr)

  fri <- fr
  rs <- fri

  # begin HTML scrape area
  AAIISENTIMENT.URL2 <- "https://www.aaii.com/sentimentsurvey/sent_results"
  DestFile  <- paste0(normalizePath(getOption("econModel.getSymbols.AAIISentiment.folder"), winslash = "/"), "/", "sent_results.html")
  if(!file.exists(DestFile)) {
    PageOut <- fetchInternet(AAIISENTIMENT.URL2, Structure = "Page", DestFile = DestFile)
    cat(PageOut, file = DestFile)
  }

  rt <- suppressMessages(htmltab::htmltab(
    doc = DestFile,
    which = '//*[@id="page_content"]/table[1]',
    rm_nodata_cols = F
  ))

  if (verbose) {
    cat("Reading disk file ", DestFile, ".....\n\n")
  }

  # I JUST NEED THE TOP ROW ( THE REST I GET FROM EXCEL )
  rt <- rt[1,,drop = FALSE]
  rt[["Reported Date"]] <- gsub(":", "", rt[["Reported Date"]])
  IndexRecent <- zoo::as.Date(rt[["Reported Date"]], format = "%B %d")
  rt <- rt[,!colnames(rt) %in% "Reported Date", drop = FALSE]
  DataColnames <- colnames(rt)
  DataRecent <- as.numeric(gsub("%", "", unlist(rt)))/100.0
  rt <- xts(matrix(DataRecent, ncol = length(DataColnames)), IndexRecent); colnames(rt) <- DataColnames

  # end html scrape area

  # prepare to splice

  # rs coredata(c)
  rsc <- colnames(rs)[!colnames(rs) %in% colnames(rt)]
  rsc <- as.list(rsc)
  Names(rsc) <- unlist(rsc)
  NamesRSC <- Names(rsc)

  te  <- unlist(lapply(rsc, function(x) NA_real_))
  rsc <- matrix(te, ncol = length(NamesRSC), dimnames = list(list(),NamesRSC))

  rtc <- cbind(zoo::coredata(rt), rsc)
  rtc <- rtc[,cSort(colnames(rtc), InitOrder = colnames(rs)), drop = FALSE]

  # Brute Force Method
  # round forward to the next available Friday
  # if Mon-Thu
  if(DescTools::Weekday(zoo::index(rt)) != 5L && DescTools::Weekday(zoo::index(rt)) < 5L) {
    zoo::index(rt) <- zoo::index(rt) + ( 5 - DescTools::Weekday(zoo::index(rt)) )
  } else if (DescTools::Weekday(zoo::index(rt)) != 5L && DescTools::Weekday(zoo::index(rt)) == 6L) {
    # if Sat
    zoo::index(rt) <- zoo::index(rt) + 6L
  } else  if (DescTools::Weekday(zoo::index(rt)) != 5L && DescTools::Weekday(zoo::index(rt)) == 7L){
    # if Sun
    zoo::index(rt) <- zoo::index(rt) + 5L
  }

  rt  <- xts::xts(rtc, zoo::index(rt))

  # splice

  rst <- rbind(rs,rt)
  # remove the first(earliest) [if any] duplicate index
  rst <- rst[!duplicated(zoo::index(rst), fromLast = TRUE),]
  fr <- rst

  fri <- fr # pass-through on "all columns in one xts object"

  # decompose [if any] into [many] Symbol(s), then return
  for (i in seq_along(Symbols)) {

    if (verbose) {
      cat("Selecting ", Symbols[[i]], ".....\n\n")
    }
    # User only wants an individual column
    if(Symbols[[i]] != "AAIISentimentSurveyData") {
      fri <- fr[, Symbols[[i]]]
    }
    fri <- quantmod___convert.time.series(fr = fri, return.class = return.class)
    if (auto.assign)
      assign(Symbols[[i]], fri, env)
  }

  Sys.setenv(TZ=oldtz)

  if (auto.assign)
    return(Symbols)
  return(fri)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Survey of Professional Forecasters data from the Philadelphia Federal Researve Board of Governors (FED)
#'
#' @description
#' About the data, file Structure: Column Header Nomenclature and Forecast Horizons: ("1") through ("6") are quarterly forecasts and ("A") and ("B") annual forecasts.
#'
#' The number ("1") represents the forecast of the quarter prior to the quarter in
#' which the survey is conducted. The forecasters know the values of the variables for this
#' quarter at the time they submit their projections in comparison to official revisions(corrections) of the official data.
#' The forecasters are permitted to forecast a revision.
#'
#' The number ("2") represents the forecast for the current quarter, defined as
#' the quarter in which the survey is conducted. The numbers ("3") through ("6")
#' represent the forecasts for the four quarters after the current quarter.
#' The letters ("A") and ("B") represent annual average forecasts
#' for the current year (the year in which the survey is conducted) and
#' the following year.
#'
#' INDUSTRY means: financial service provider ("1"), non-financial service provider ("2"), or do not know the industry ("3").  INDUSTRY is not shown in the output data.
#'
#' About PR*, probability forecasts, read the documentation. This is "very" complicated.
#'
#' About the date/time index, deadlines for responses are at late in the second week of the middle month of each quarter.
#' This report is released to the public in the second week of of the middle month of each quarter.
#' True deadline and news release dates for surveys prior to 1990:Q2 are not known.
#'
#' Therefore, for simplicity in this R function, the index date is estimated to be at the mid-point of the quarter.
#'
#' The 1990Q2 survey was not taken in real time, because the Philadelphia Fed
#' had not yet taken over the survey. Forecasters were asked to provide dated
#' forecasts from May 1990.
#' The 1996Q1 survey was delayed because of the federal government shutdown,
#' which in turn delayed the release of government statistical data.
#'
#' The 2013Q4 survey was delayed because of the federal government shutdown,
#' which in turn delayed the release of government statistical data.
#' @note
#' \preformatted{
#'
#' Some, compare and contrasts, may be the
#' forecasters prediction v.s. FRED data of  what actually happened.
#'
#' # unemployment
#'
#' Civilian Unemployment Rate (UNEMP)
#' https://www.philadelphiafed.org/research-and-data/real-time-center
#'        /survey-of-professional-forecasters/data-files/unemp
#'
#' Civilian Unemployment Rate (UNRATE)
#' https://fred.stlouisfed.org/series/UNRATE/
#'
#' # prices ( e.g. inflation )
#'
#' Price Index for Gross National Product/Gross Domestic Product (PGDP)
#' https://www.philadelphiafed.org/research-and-data/real-time-center
#'        /survey-of-professional-forecasters/data-files/pgdp
#'
#' Gross National Product (chain-type price index) (A001RV1Q225SBEA)
#' https://fred.stlouisfed.org/series/A001RV1Q225SBEA
#'
#' # gross domestic product
#'
#' Nominal Gross National Product/Gross Domestic Product (NGDP)
#' https://www.philadelphiafed.org/research-and-data/real-time-center
#'        /survey-of-professional-forecasters/data-files/ngdp
#'
#' Gross Domestic Product (GDP)
#' https://fred.stlouisfed.org/series/GDP
#'
#' # corporate Profits
#'
#' Corporate Profits After Tax (CPROF)
#' https://www.philadelphiafed.org/research-and-data/real-time-center
#'        /survey-of-professional-forecasters/data-files/cprof
#'
#' Corporate Profits After Tax (without IVA and CCAdj) (CP)
#' https://fred.stlouisfed.org/series/CP
#' }
#' @param Symbols  A character vector.  Required. Specifies the names of each symbol to be loaded
#' To get all of the data use: "USFedPhilForecastsData".  Possible individual
#' Symbols are listed in the Forecasters documentation: (1) Historical Data Files for the Survey
#' of Professional Forecasters (2) Documentation: PDF Provides information on
#' all variables, transformations, and files in the survey.
#' E.g. "UNEMP". Symbols will be returned as (xts) data.
#' @param env Environment. Default is the global environment. Where to create objects.
#' @param return.class. String. Desired class of returned object.  Default is "xts".
#' The value can be "zoo", "data.frame", or "xts".
#' @param Fun Function.  Default is mean. This is the aggregating function.  NA data cells are automatically removed from the data before the (remaining) data is sent to the aggregating function.
#' @param ... Dots passed to Fun.
#' @return A call to getSymbols.USFedPhilForecasts will load into the specified
#' environment one object for each \code{Symbol} specified,
#' with class defined by \code{return.class}.
#' @author Andre Mikulec
#' @references
#' \cite{Dates of deadlines for surveys from 1990:Q2 to the present (BROKEN LINK)
#' \url{https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-ofprofessional-forecasters/spf-release-dates.txt?la=en}
#' }
#' @references
#' \cite{Survey of Professional Forecasters
#' \url{https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/survey-of-professional-forecasters}
#' }
#' @references
#' \cite{Calendar of Events (Release(Publishing) Dates)
#' \url{https://www.philadelphiafed.org/calendar-of-events?release=survey-of-professional-forecasters#Economic-Release-Calendar}
#' }
#' @references
#' \cite{Data Files - Survey of Professional Forecasters
#' \url{https://www.philadelphiafed.org/surveys-and-data/data-files}
#' }
#' @references
#' \cite{Surveys 1968:4–present, Individual Forecasts: Survey of Professional Forecasters
#' \url{https://www.philadelphiafed.org/-/media/frbp/assets/surveys-and-data/survey-of-professional-forecasters/historical-data/spfmicrodata.xlsx?la=en}
#' }
#' @references
#' \cite{Caveats on Identification Numbers for Individual Forecasts, Individual Forecasts: Survey of Professional Forecasters
#' \url{https://www.philadelphiafed.org/-/media/frbp/assets/surveys-and-data/survey-of-professional-forecasters/spf-caveats.pdf?la=en}
#' }
#' @references
#' \cite{Documentation, Individual Forecasts: Survey of Professional Forecasters
#' \url{https://www.philadelphiafed.org/-/media/frbp/assets/surveys-and-data/survey-of-professional-forecasters/spf-documentation.pdf?la=en}
#' }
#' @references
#' \cite{Individual Forecasts: Survey of Professional Forecasters
#' \url{https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/individual-forecasts}
#' }
#' @references
#' \cite{Dean Croushore, Introducing The Survey of Professional Forecasters, Business review (Federal Reserve Bank of Philadelphia), 6(Nov):3-15
#' \url{https://www.researchgate.net/profile/Dean_Croushore/publication/5051610_Introducing_The_Survey_of_Professional_Forecasters/links/54369c5a0cf2dc341db3684b/Introducing-The-Survey-of-Professional-Forecasters.pdf}
#' }
#' @references
#' \cite{Introducing: The Survey of Professional Forecasters
#' \url{https://www.researchgate.net/publication/5051610_Introducing_The_Survey_of_Professional_Forecasters}
#' }
#' @examples
#' \dontrun{
#' # common usage
#' # mean(the average) of the forecasts
#' getSymbols(c("UNEMP3", "INDPROD2"), src = "USFedPhilForecasts")
#'
#' # standard deviation of forecasts
#' getSymbols("UNEMP3", src = "USFedPhilForecasts", Fun = sd, verbose = T)
#'
#' # number of forecasters
#' getSymbols("UNEMP3", src = "USFedPhilForecasts", Fun = length)
#'
#' # all columns in one VERY WIDE xts object
#' # wait 5 minutes (creates the .fst files)
#' getSymbols("USFedPhilForecastsData", src = "USFedPhilForecasts", verbose = T)
#'
#' # wait 30 seconds
#' getSymbols("USFedPhilForecastsData", src = "USFedPhilForecasts", verbose = T)
#'}
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom readxl read_xls excel_sheets
#' @importFrom fst read.fst write.fst
#' @importFrom RQuantLib adjust
#' @importFrom zoo index as.Date
#' @importFrom xts xts
#' @importFrom data.table rbindlist
#' @importFrom DescTools DoCall
#' @export
getSymbols.USFedPhilForecasts <- function(Symbols, env, return.class = "xts",
                                          Fun = mean, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  # if not done elsewhere
  #correct for TZ
  oldtz <- Sys.getenv("TZ")
  if(oldtz!="UTC") {
    Sys.setenv(TZ="UTC")
  }

  # determine temp folder
  ops <- options()
  if(!"econModel.getSymbols.USFedPhilForecasts.folder" %in% Names(ops)) {
    options(append(ops, list(econModel.getSymbols.USFedPhilForecasts.folder = paste0(normalizePath(tempdir(), winslash = "/"), "/", "econModel.getSymbols.USFedPhilForecasts.folder"))))
  }

  # verify/create temp folder
  if(!dir.exists(getOption("econModel.getSymbols.USFedPhilForecasts.folder"))) {
    dir.create(getOption("econModel.getSymbols.USFedPhilForecasts.folder"))
  }

  quantmod::importDefaults("getSymbols.USFedPhilForecasts")

  this.env <- environment()
  for (var in names(list(...))) {
    assign(var, list(...)[[var]], this.env)
  }
  if (!hasArg("verbose"))
    verbose <- FALSE
  if (!hasArg("auto.assign"))
    auto.assign <- TRUE

  Fun <- match.fun(Fun)

  # begin xls area
  USFEDPHILFORECASTS.URL <- "https://www.philadelphiafed.org/-/media/frbp/assets/surveys-and-data/survey-of-professional-forecasters/historical-data/spfmicrodata.xlsx?la=en"
  DestFile  <- paste0(normalizePath(getOption("econModel.getSymbols.USFedPhilForecasts.folder"), winslash = "/"), "/", "SPFmicrodata.xlsx")
  if(!file.exists(DestFile)) {
    DirFilePath <- fetchInternet(USFEDPHILFORECASTS.URL, Structure = "BinFile", DestFile = DestFile)
  }

  Sheets <- readxl::excel_sheets(DestFile)

  # save resources
  # looking for potential sheets that may? have those Symbols
  if(!all(unique(Symbols) %in% "USFedPhilForecastsData")) {
    SheetsIndexes <- c(list(), unique(unlist(lapply(Symbols, function(x) {agrep(x, Sheets)}))))
    if(length(SheetsIndexes)){
      Sheets <- Sheets[unlist(SheetsIndexes)]
    } else {
      stop(paste0("Symbol(s) ", paste0(Symbols, collapse = " and "), " is/are not found"))
    }
  }

  FR <- list(xts::xts(,zoo::as.Date(0)[0]))
  # could have lapply-ed this
  for(Sheet in Sheets) {

    if(!file.exists(paste0(normalizePath(getOption("econModel.getSymbols.USFedPhilForecasts.folder"), winslash = "/"), "/",  Sheet, ".fst"))) {

      # data.frame
      # once only
      fr <- suppressWarnings(readxl::read_xlsx(path = DestFile, sheet = Sheet,
                                               col_names = T, # get the Excel sheet row 1 column names
                                               col_types = "numeric")) # recycled

      if (verbose) {
        cat("Reading (of the one and only time time) disk file:\n", DestFile, ".....\n\n")
      }

      # once only
      fst::write.fst(fr, path = paste0(normalizePath(getOption("econModel.getSymbols.USFedPhilForecasts.folder"), winslash = "/"), "/",  Sheet, ".fst"), compress = 100L)

      if (verbose) {
        cat("Writing (of the one and only time) disk file:\n", paste0(normalizePath(getOption("econModel.getSymbols.USFedPhilForecasts.folder"), winslash = "/"), "/",  Sheet, ".fst", ".....\n\n"))
      }

    } else {
      fr <- fst::read.fst(paste0(normalizePath(getOption("econModel.getSymbols.USFedPhilForecasts.folder"), winslash = "/"), "/",  Sheet, ".fst"))

      if (verbose) {
        cat("Reading disk file:\n", paste0(normalizePath(getOption("econModel.getSymbols.USFedPhilForecasts.folder"), winslash = "/"), "/",  Sheet, ".fst", ".....\n\n"))
      }

    }

    # not useful
    fr <- fr[, !colnames(fr) %in% c("ID", "INDUSTRY")]
    # saved! (begin to create the index)
    SplittingIndex <- Index <- paste0(fr[["YEAR"]]," ", fr[["QUARTER"]])
    # clean
    fr <- fr[, !colnames(fr) %in% c("YEAR", "QUARTER")]

    # just my Symbol(s) of interest
    if(!any(Symbols %in% "USFedPhilForecastsData")){
      if(any(colnames(fr) %in% Symbols)) {
        fr <- fr[, colnames(fr) %in% Symbols, drop = F]
      } else {
        # no columns are found

        if (verbose)
          cat(paste0( "Skipping using Excel sheet ", Sheet,".\n\n"))

        next # for loop
      }
    }

    # continue to create the index
    Index <- unique(Index)
    Index <- zoo::as.yearqtr(Index, format ="%Y %q")
    # half way through the quarter
    Index <- zoo::as.Date(Index, frac = 0.5)
    # if lands on a holiday(or weekend) adjust forward to the following day
    Index <- RQuantLib::adjust("UnitedStates/GovernmentBond", Index, 1L)

    # index of x (ix)
    fr <- data.table::rbindlist(lapply(split(fr, f = SplittingIndex), function(ix) {

      # column of x (cx)
      # unlist() eats numeric(0)
      #   instead use "not unlist" and NA_real_
      data.frame(lapply(ix, function(cx){
        cx <- cx[!is.na(cx)]
        if(!NROW(cx)) {
          return(NA_real_)
        } else {
          # Future use
          # Args <- as.list(args(Fun))
          # Args <- Args[sapply(Args, function(x) !is.null(x))]
          # Names(Args)
          #
          # Current use
          # just strip out getSymbols "warnings" and ".has1sym."
          return(DescTools::DoCall(Fun, c(list(), list(cx), Dots[!Names(Dots) %in% c("verbose","auto.assign","warnings",".has1sym.")])))
        }
      }))

    }), fill = T)

    fr <- xts::xts(as.matrix(fr), Index,
                   src = "USFedPhilForecasts", updated = Sys.time())

    if (verbose)
      cat(paste0( "Done with work using Excel sheet ", Sheet,".\n\n"))

    FR <- c(list(), FR, list(fr))

  } # for Sheet in Sheets

  fr <- DescTools::DoCall(cbind, FR)

  fri <- fr # pass-throught on "USFedPhilForecastsData"

  # decompose [if any] into [many] Symbol(s), then return
  for (i in seq_along(Symbols)) {

    if (verbose) {
      cat("Selecting (Excel column) Symbol ", Symbols[[i]], ".....\n\n")
    }
    # User only wants an individual column
    if(Symbols[[i]] != "USFedPhilForecastsData") {
      fri <- fr[, Symbols[[i]]]
    }
    fri <- quantmod___convert.time.series(fr = fri, return.class = return.class)
    if (auto.assign)
      assign(Symbols[[i]], fri, env)
  }

  Sys.setenv(TZ=oldtz)

  if (auto.assign)
    return(Symbols)
  return(fri)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}





#' Return the Correct Elements of A Data Object
#'
#' @description
#' A function to return the rows of a vector or two-dimensional data object.
#' This is a wrapper over the R CRAN package econModel first and last.
#' Different here is that if n elements can not be returned, then instead, stop() happens.
#'
#' @param x 1 or 2 dimensional data object.
#' @param ... Dots passed.
#' @return first elements or zero length element
#' @importFrom tryCatchLog tryCatchLog
#' @export
relativeRows <- function(x, ...) {
tryCatchLog::tryCatchLog({

  # dangerous assumption
  # function (x, n = 1, keep = FALSE, ...)
  Dots <- list(...)
  if(
    # default (probaly ok/safe to call!?!)
    # relativeRows(x, n = 1)
    #
    # call first(x, 1) # dangerous assumption # first(x, n = 2) (not default)
    (length(Dots) && (!"n" %in% Names(Dots)) && NROW(x) < Dots[[1]])
    ||
    # call first(x, n = 2L) # correct way to call (not the default)
    ("n" %in% Names(Dots)) && (NROW(x) < Dots[["n"]])
  ){
    stop(paste0("Function relativeRows can not return exactly ", Dots[["n"]], " elements."))
  } else {
    return(x)
  }
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Return First n Elements of A Data Object
#'
#' @description
#' A generic function to return the first or first elements or rows of a vector or two-dimensional data object.
#' This is a wrapper over the R CRAN package xts S3 function first.
#'
#' Different here is that if n elements can not be returned, then instead, zero elements are returned.
#'
#' @param x 1 or 2 dimensional data object.
#' @param ... Dots passed. Typically, n = the exact number of elements to return.
#' @return first elements or zero length element
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom xts first
#' @export
first <- function(x, ...) {
tryCatchLog::tryCatchLog({

  # dangerous assumption
  # function (x, n = 1, keep = FALSE, ...)
  x <- xts::first(x, ...)

  relativeRows(x, ...)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Tries to Return First n Elements of A Data Object
#'
#' @description
#' A generic function to return the first or first elements or rows of a vector or two-dimensional data object.
#' This is a wrapper over the R CRAN package xts S3 function first.
#'
#' It tries to return (if available) up to "n" elements
#'
#' @param x 1 or 2 dimensional data object.
#' @param ... Dots passed. Typically, tries to gather up to "n" elements to return.
#' @return first elements or zero length element
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom xts first
#' @export
firstly <- function(x, ...) {
tryCatchLog::tryCatchLog({

  # dangerous assumption
  # function (x, n = 1, keep = FALSE, ...)
  x <- xts::first(x, ...)


}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}






#' Return Last n Elements of A Data Object
#'
#' @description
#' A generic function to return the last or last elements or rows of a vector or two-dimensional data object.
#' This is a wrapper over the R CRAN package xts S3 function last.
#'
#' Different here is that if n elements can not be returned, then instead, zero elements are returned.
#'
#' @param x 1 or 2 dimensional data object.
#' @param ... Dots passed.  Typically, n = the exact number of elements to return.
#' @return last elements or zero length element
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom xts last
#' @export
last <- function(x, ...) {
tryCatchLog::tryCatchLog({

  # dangerous assumption
  # function (x, n = 1, keep = FALSE, ...)
  x <- xts::last(x, ...)

  relativeRows(x, ...)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Tries to Return Last n Elements of A Data Object
#'
#' @description
#' A generic function to return the last or last elements or rows of a vector or two-dimensional data object.
#' This is a wrapper over the R CRAN package xts S3 function last.
#'
#' It tries to return (if available) up to "n" elements.
#'
#' @param x 1 or 2 dimensional data object.
#' @param ... Dots passed. Typically, tries to gather up to "n" elements to return.
#' @return last elements or zero length element
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom xts last
#' @export
lastly <- function(x, ...) {
tryCatchLog::tryCatchLog({

  # dangerous assumption
  # function (x, n = 1, keep = FALSE, ...)
  x <- xts::last(x, ...)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}




#' Scrape or Download a File from the Internet
#'
#' Returned data is either a page, collection of vector lines, a connection, or a file.
#'
#' @param x URL
#' @param Structure String. Default is "Page". Return the data as a page.  Other is "Lines" to return the result as a group of lines.  Another is "BinFile" to download a file; parameter "DestFile" is required.
#' @param Collapse String. Default is the newline escape character. If Structure == "Page" then separate the lines by the newline escape character. Alternately, separate the lines by a single character Collapse.  Another choice may be " ". If Structure != "Page", then this parameter is ignored.
#' @param encode Logical. Default is TRUE. Perform URLencoding upon URL.
#' @param Message String. Default is paste0("function ", "fetchInternet").  Message is appended to the user agent.
#' @param conOut Logical.  Default is FALSE. If TRUE, instead return immediately the connection object.  This ignores (and does not do) all Structure and Collapse processing.
#' @param DestFile String. Default none. Required only when using "Structure = "BinFile".  Ignored otherwise. Path and File destination.
#' @param Quiet Logical.  Default is TRUE.  Only used when using "Structure = "BinFile".
#' @param Mode String. Default is "wb". Only used when using "Structure = "BinFile".
#' @param ... Dots. Passed to URLencode
#' @return just the connecion object(conOut = T) or the one element page(Page) or many vectors of lines(Lines)
#' @examples
#' \dontrun{
#'
#' writeLines(fetchInternet("https://fred.stlouisfed.org/data/RECPROUSM156N.txt"))
#' Title:               Smoothed U.S. Recession Probabilities
#' Series ID:           RECPROUSM156N
#' Source:              Piger, Jeremy Max, Chauvet, Marcelle
#' Release:             U.S. Recession Probabilities
#' Seasonal Adjustment: Not Seasonally Adjusted
#' Frequency:           Monthly
#' Units:               Percent
#' Date Range:          1967-06-01 to 2020-09-01
#' Last Updated:        2020-11-02 7:01 AM CST
#'
#' fetchInternet("https://fred.stlouisfed.org/data/RECPROUSM156N.txt", Structure = "Lines")
#'
#' # tilde expansion (writes NOT HERE but to the users HOME directory)
#' DirFilePath <- fetchInternet("http://www.econ.yale.edu/~shiller/data/ie_data.xls",
#'   Structure = "BinFile", DestFile = "~/HERE.xls")
#'
#' str(DirFilePath)
#' chr "C:\\APPLICATIONS\\R-4.0._\\R_USER_4.0\\HERE.xls"
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom utils URLencode
#' @importFrom curl curl_version new_handle handle_setopt curl handle_reset curl_download
fetchInternet <- function(x, Structure = "Page", Collapse = "\n",
                          encode = T, Message = paste0("function ", "fetchInternet"),
                          conOut = F, DestFile, Quiet = T, Mode = "wb", ...) {
tryCatchLog::tryCatchLog({

 if(Structure == "BinFile") {
   if(missing(DestFile)) {
     stop("Requesting Structure \"BinFile\" but needs missing \"DestFile\".")
   }
 }

  if(encode) {
    URL <- utils::URLencode(x, ...)
  } else {
    URL <- x
  }

  # scrape
  h <- curl::new_handle()
  useragent <- paste("curl/", curl::curl_version()$version, " ", Message, " of R CRAN package econModel calling function curl of R CRAN package curl", sep = "")
  # debug in Fiddler 4
  # curl --proxy 127.0.0.1:8888 --insecure -A "custom agent" https://alfred.stlouisfed.org/series/downloaddata?seid=GDP
  # Body dropped from POST request when using proxy with NTLM authentication #146
  # https://github.com/jeroen/curl/issues/146
  # curl::handle_setopt(h, .list = list(proxy = "127.0.0.1", proxyport = 8888, useragent = useragent))
  curl::handle_setopt(h, .list = list(useragent = useragent))
  if(Structure %in% c("Lines", "Page")) {
    con <- curl::curl(URL, handle = h)
  }

  # download
  if(Structure == "BinFile") {
    # Value: Path of downloaded file (invisibly).
    # Always (and when an error occure) returns: DirFilePath DestFile
    # If fails to write to "desfile" (e.g. the destination path is "not possible")
    #    then will error HERE.
    DirFilePath <- curl::curl_download(URL, destfile = DestFile, quiet = Quiet, mode = Mode, handle = h)
  }
  # docs say that it does not do much
  curl::handle_reset(h)

  if(conOut && exists("con")) {
    # calling program is now reponsible to do "close(con)"
    return(con)
  }

  # format output
  if(Structure %in% c("Lines", "Page")) {
    Lines <- readLines(con)
  }
  if(exists("con")) {
    close(con)
  }

  if(Structure == "Lines"){
    Result <- Lines
  }
  if(Structure == "Page") {
    Result <- paste0(Lines, collapse = Collapse)
  }
  if(Structure == "BinFile") {
    Result <- DirFilePath
  }
  Result

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}





#' Download attributes of Times Series of Federal Reserve Economic Data - FRED(R)
#'
#' Data typically includes the Title, Series ID, Source, Release, Seasonal Adjustment, Frequency, Units, Date Range, Last Updated, and Notes.
#'
#' @param Symbol specifying the name of the symbol to be queried
#' @param DataSheet Logical. Default is FALSE.  Do not return the data payload. Otherwise, if TRUE, then also return the data payload of all of the data in an R attribute named "DataSheet".
#' @return data.frame of attributes
#' @examples
#' \dontrun{
#'
#' # Smoothed U.S. Recession Probabilities
#' # Source: Piger, Jeremy Max, Chauvet, Marcelle
#' # https://fred.stlouisfed.org/data/RECPROUSM156N.txt
#' #
#' print(str(fredAttributes("RECPROUSM156N")))
#'
#' 'data.frame':	1 obs. of  10 variables:
#' $ Title             : chr "Smoothed U.S. Recession Probabilities"
#' $ SeriesID          : chr "RECPROUSM156N"
#' $ Source            : chr "Piger, Jeremy Max, Chauvet, Marcelle"
#' $ Release           : chr "U.S. Recession Probabilities"
#' $ SeasonalAdjustment: chr "Not Seasonally Adjusted"
#' $ Frequency         : chr "Monthly"
#' $ Units             : chr "Percent"
#' $ DateRange         : chr "1967-06-01 to 2020-09-01"
#' $ LastUpdated       : chr "2020-11-02 7:01 AM CST"
#' $ Notes             : chr "Smoothed recession probabilities for the United"
#'
#' tail(attr(fredAttributes("RECPROUSM156N", DataSheet = T), "DataSheet"))
#' RECPROUSM156N
#' 2020-04-01        100.00
#' 2020-05-01         37.85
#' 2020-06-01         37.85
#' 2020-07-01         47.92
#' 2020-08-01        100.00
#' 2020-09-01        100.00
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom curl curl_version new_handle handle_setopt curl handle_reset
fredAttributes <-function(Symbol, DataSheet = F) {
tryCatchLog::tryCatchLog({

  if(is.null(Symbol)) stop("Symbol can not be NULL.")

  # get the meta-data
  FRED.URL <- "https://fred.stlouisfed.org/data/"
  URL <- paste0(FRED.URL, Symbol, ".txt")

  # # scrape
  # h <- curl::new_handle()
  # useragent <- paste("curl/", curl::curl_version()$version, " function fredAttributes of R CRAN package econModel calling function curl of R CRAN package curl", sep = "")
  # # debug in Fiddler 4
  # # curl --proxy 127.0.0.1:8888 --insecure -A "custom agent" https://alfred.stlouisfed.org/series/downloaddata?seid=GDP
  # # Body dropped from POST request when using proxy with NTLM authentication #146
  # # https://github.com/jeroen/curl/issues/146
  # # curl::handle_setopt(h, .list = list(proxy = "127.0.0.1", proxyport = 8888, useragent = useragent))
  # curl::handle_setopt(h, .list = list(useragent = useragent))
  # # go for it
  # # Page <- paste0(scan(URL, what = character()), collapse = " ")
  # con <- curl::curl(URL, handle = h)
  # Lines <- readLines(con)
  # # docs say that it does not do much
  # curl::handle_reset(h)
  # close(con)

  fres <- Lines <- fetchInternet(URL, Structure = "Lines")

  # boundary splitter between header area and data area
  BoHeaderArea <- 1
  EoHeaderArea <- match(TRUE, grepl("^DATE.*VALUE$", fres)) - 1L
  BoDataArea   <- EoHeaderArea + 2L
  EoDataArea   <- length(fres)
  HeaderArea <- fres[seq(BoHeaderArea,EoHeaderArea,1)]

  # read.dcf sometimes does not likes lines with blanks
  HeaderArea <- HeaderArea[!grepl("^[[:blank:]]+$|^$", HeaderArea)]

  # collect information about the series
  tcon <- textConnection(paste0(HeaderArea, collapse = "\n"))
  # try: keep.white = "Notes"

  # SeriesInfo Xts attributes
  te <- read.dcf(tcon, keep.white = "Notes")
  te <- as.data.frame(te, stringsAsFactors = FALSE)
  colnames(te) <- gsub("\\s" , "",  colnames(te))
  SeriesInfo <- te

  close(tcon)

  if(DataSheet == T) {

    DataArea   <- fres[seq(BoDataArea,EoDataArea ,1)]

    # separate dates and values
    DatesAndValues <- strsplit(DataArea, "[[:blank:]]+")
    #
    # idea from
    #
    # Select first element of nested list
    # MAR 2017
    # https://stackoverflow.com/questions/20428742/select-first-element-of-nested-list
    #
    DatesAndValues       <- unlist(DatesAndValues)
    DatesAndValuesLength <- length(DatesAndValues)
    # every other one
    Dates  <- DatesAndValues[seq(1,DatesAndValuesLength,2)]
    Values <- DatesAndValues[seq(2,DatesAndValuesLength,2)]
    Values[Values %in% "."] <- NA

    # columns are assigned in reverse order
    Payload <-  as.data.frame(Values, Dates)
    colnames(Payload)[1] <- Symbol
    attributes(SeriesInfo)$DataSheet <- Payload
  }

  SeriesInfo
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


#' Determine LastUpdated(Publishing) Dates
#'
#' Estimate the time distance between observations Dates/times and their corresponding LastUpdated(Publishing) Dates/times.
#'
#' @param x Date times of observation dates
#' @param Calendar Default is "UnitedStates/GovernmentBond". Calendar to use.  See ?? RQuantLib::Calendars
#' @param Frequency No default (required). Values can be Daily(not implemented yet), Weekly(not implemented yet), Monthly, and Quarterly
#' @param LastUpdated Date time.  Default NULL.  Date time of the published date of the newest(latest) observation
#' @param NdayInMonth Integer.  Default NULL.  This only applied to the Frequency of "Quarterly" or "Monthly". If this parameter is present, then this parameter provides additiona information to parameter LastUpdated.  Use this parameter NdayInMonth to as the Day after the Reference period to estimate.  See ? timeDate::timeNthNdayInMonth
#' @param LastOfDateRange Default NULL.  If not present, this value will be taken from the last observation of x.  This parameter value represents the earliest of the time between an observation and its corresponding LastUpdated Date time.
#' @return vector of Date times
#' @references
#' \cite{Schedule of Releases for the Employment Situation
#' \url{https://www.bls.gov/schedule/news_release/empsit.htm}}
#' @examples
#' \dontrun{
#' Title:               Gross Domestic Product
#' Series ID:           GDP
#' Source:              U.S. Bureau of Economic Analysis
#' Release:             Gross Domestic Product
#' Seasonal Adjustment: Seasonally Adjusted Annual Rate
#' Frequency:           Quarterly
#' Units:               Billions of Dollars
#' Date Range:          1947-01-01 to 2020-07-01
#' Last Updated:        2020-10-29 7:52 AM CDT
#'
#' atr <- fredAttributes("GDP")
#'
#' library(quantmod)
#'
#' getSymbols("GDP", src = "FRED")
#'
#' estimLastUpdated(index(getSymbols("GDP")),
#'   Frequency = atr$Frequency,
#'   LastUpdated = atr$LastUpdated,
#'   LastOfDateRange = tail(strsplit(atr$DateRange, " to "),1)
#' )
#'
#' Title:               Unemployment Rate
#' Series ID:           UNRATE
#' Source:              U.S. Bureau of Labor Statistics
#' Release:             Employment Situation
#' Seasonal Adjustment: Seasonally Adjusted
#' Frequency:           Monthly
#' Units:               Percent
#' Date Range:          1948-01-01 to 2020-10-01
#' Last Updated:        2020-11-06 7:47 AM CST
#'
#' # LastUpdated (updated) during the first Friday of every month
#' # if that "first Friday" is not available, then the
#' # the updated occurs at the next Friday
#' #
#' # Schedule of Releases for the Employment Situation
#' https://www.bls.gov/schedule/news_release/empsit.htm
#'
#' # PAY & LEAVE (UNRATE data is not released during these days)
#' https://www.opm.gov/policy-data-oversight/pay-leave/federal-holidays/#url=2020
#'
#' atr <- fredAttributes("UNRATE")
#'
#' library(quantmod)
#'
#' getSymbols("UNRATE", src = "FRED")
#'
#' # UNRATE is released on the First Friday that is
#' # not a weekend day no holiday
#' #
#' tail(estimLastUpdated(index(UNRATE),
#'   Frequency =  head(strsplit(atr$Frequency, ", ")[[1]],1),
#'   LastUpdated = atr$LastUpdated,
#'   LastOfDateRange = tail(strsplit(atr$DateRange, " to ")[[1]],1)
#' ), 4)
#'
#' [1] "2020-08-06" "2020-09-08" "2020-10-07" "2020-11-05"
#'
#' # first Friday (better)
#' tail(estimLastUpdated(index(UNRATE),
#'                  Frequency =  head(strsplit(atr$Frequency, ", ")[[1]],1),
#'                  LastUpdated = atr$LastUpdated,
#'                  NdayInMonth = 5L, # 1st Friday
#'                  LastOfDateRange = tail(strsplit(atr$DateRange, " to ")[[1]],1)
#' ), 4)
#'
#' [1] "2020-08-07" "2020-09-04" "2020-10-02" "2020-11-06"
#'
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo as.Date
#' @importFrom DescTools AddMonths
#' @importFrom data.table fifelse
#' @importFrom Hmisc truncPOSIXt
#' @importFrom timeDate timeNthNdayInMonth
#' @importFrom RQuantLib businessDaysBetween isHoliday adjust
estimLastUpdated <- function(x, Calendar = "UnitedStates/GovernmentBond",
                           Frequency, LastUpdated = NULL, NdayInMonth = NULL, LastOfDateRange = NULL
                           ) {
tryCatchLog::tryCatchLog({


  if(is.null(x))                stop("x Date series is required.")
  if(is.null(Calendar))         Calendar <- "UnitedStates/GovernmentBond"
  if(is.null(Frequency))        stop("Frequency is required.")

  if(is.null(LastUpdated)) stop("LastUpdated xor NdayInMonth is required.")
  if(is.null(LastUpdated) && !is.null(NdayInMonth)) stop("LastUpdated is still required when NdayInMonth is used.")

  if(is.null(LastOfDateRange))  {LastOfDateRange <- tail(x,1) }

  oldtz <- Sys.getenv("TZ")
  if(oldtz!="UTC") {
    Sys.setenv(TZ="UTC")
  }

  # original Date/time class
  OrigDateTimeClass <- class(x)[1]

  # time partial
  # cut.POSIXt does not have parameter format
  LastUpdatedDayTimeDiff <- as.POSIXct(LastUpdated, format = "%Y-%m-%d %I:%M %p") - Hmisc::truncPOSIXt(as.POSIXct(LastUpdated, format = "%Y-%m-%d %I:%M %p"), "days")

  if(Frequency %in%  c("Quarterly","Monthly")) {
    if(Frequency == "Quarterly") MonthsAdjust <- 3L
    if(Frequency == "Monthly")   MonthsAdjust <- 1L
    DateReference <- DescTools::AddMonths(zoo::as.Date(LastOfDateRange), MonthsAdjust)
  }
  if(Frequency %in%  c("Weekly", "Daily")) {
    DateReference <- zoo::as.Date(LastOfDateRange)
  }

  # will not be used(be overridden) if using NdayInMonth
  WorkDaysAfterBeginOfDateReference <- RQuantLib::businessDaysBetween(Calendar, from = DateReference, to = zoo::as.Date(LastUpdated))


  if(Frequency %in%  c("Quarterly","Monthly")) {

    # always ran
    NewDatesReferences <- DescTools::AddMonths(zoo::as.Date(x), MonthsAdjust)

    if(!is.null(NdayInMonth)) {
      NewDatesReferences <- as.Date(timeDate::timeNthNdayInMonth(as.character(NewDatesReferences), nday = NdayInMonth, nth = 1, format = "%Y-%m-%d"))
      # if a holiday, then try next week (7 days later)
      NewDatesReferences <- NewDatesReferences + data.table::fifelse(RQuantLib::isHoliday(Calendar, NewDatesReferences), 7L, 0L)
      WorkDaysAfterBeginOfDateReference <- 0L
    }
  }
  if(Frequency %in%  c("Weekly", "Daily")) {
    NewDatesReferences <- zoo::as.Date(x)
  }
  x <- RQuantLib::advance(Calendar, NewDatesReferences, WorkDaysAfterBeginOfDateReference, 0) # 0 Days

  # add back the time partial
  x <- Hmisc::truncPOSIXt(as.POSIXct(x), "days") + LastUpdatedDayTimeDiff

  # put back the original Date/time class
  x <- eval(parse(text = paste0("as.", OrigDateTimeClass, "(x)")))

  # less direct with UNRATE (R CRAN package timeDate) may help
  # two(2) cases
  # 1st Friday Monthly (UNRATE)
  # 15th Monthly (OTHERS)

  Sys.setenv(TZ=oldtz)
  x

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}









#' Download US Treasury Yield Curve Data
#'
#' @description
#' Downloads US Treasury yield curve data from the US Treasury web site.
#' This is a fix and heavy re-write (using regular expressions instead of XML) of R CRAN package ustyc function getYieldCurve.
#' What is different is the following:
#' (1) This function works on all of the treasury rate data found at \url{https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics}: the R CRAN package ustyc function getYieldCurve only works on "Daily Treasury Yield Curve Rates".
#' (2) The function defaults are different than in the R CRAN package ustyc function getYieldCurve.
#' (3) Instead of using functions from the R CRAN package "XML", because of the "need for speed", the R package "base" PCRE2 regular expression functions are used.
#' (4) Interprets (with limits) from the (rich datatype) XML namespaces of http://schemas.microsoft.com/ado/2007/08/dataservices/metadata and http://schemas.microsoft.com/ado/2007/08/dataservices) into R.
#'
#' @param base String. Required. Default is the URL: See args(yieldCurve). The base URL for the data service, defaulting to http://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData.
#' @param year String or Numeric. Default is none. The desired year number or NULL for all years. If none then the default is the current year.
#' @param month	String or Numeric.  Default is none. The desired month number in the format (01(or 1) through 12) of Jan-Dec, or NULL for all months. If none then the default is the current month.
#' @param IndexName String. Default is none. The column IndexName to be used in the URL.  This is specific and in very different per each URL.  See the examples.  If none, then default is "NEW_DATE".
#' @return xts object
#' @author Andre Mikulec (re-write)
#' @author Matt Barry (original)
#' @references
#' \cite{GetYieldCurve - XML content does not seem to be XML #1
#' \url{https://github.com/mrbcuda/ustyc/issues/1}
#' }
#' @references
#' \cite{Unknown IO error #2
#' \url{https://github.com/mrbcuda/ustyc/issues/2}
#' }
#' @references
#' \cite{Interest Rate Statistics
#' \url{https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics}
#' }
#' @examples
#' \dontrun{
#' # (1) Daily Treasury Yield Curve Rates
#' # https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yield
#' # https://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData?$filter=month(NEW_DATE)%20eq%2012%20and%20year(NEW_DATE)%20eq%202020"
#' result <- yieldCurve()
#' # if the current date is in DEC 2020 (then the next example is the same)
#' result <- yieldCurve(year = 2020, month = 12)
#' result
#'
#' # (1.5) Everything now and in history of Daily Treasury Yield Curve Rates
#' # https://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData
#' # result <- yieldCurve(year = NULL, month = NULL)
#' result
#'
#' # (2) Daily Treasury Bill Rates Data
#' # https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=billrates
#' # https://data.treasury.gov/feed.svc/DailyTreasuryBillRateData?$filter=month(INDEX_DATE)%20eq%2012%20and%20year(INDEX_DATE)%20eq%202020
#' result <- yieldCurve("https://data.treasury.gov/feed.svc/DailyTreasuryBillRateData",
#'             IndexName = "INDEX_DATE")
#' result
#'
#' # (3) Daily Treasury Long Term Rate Data
#' # https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=longtermrate
#' # http://data.treasury.gov/feed.svc/DailyTreasuryLongTermRateData?$filter=month(QUOTE_DATE)%20eq%2012%20and%20year(QUOTE_DATE)%20eq%202020"
#' result <- yieldCurve("http://data.treasury.gov/feed.svc/DailyTreasuryLongTermRateData",
#'             IndexName = "QUOTE_DATE")
#' result
#'
#' # (4) Daily Treasury Real Yield Curve Rates
#' # https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=realyield
#' # https://data.treasury.gov/feed.svc/DailyTreasuryRealYieldCurveRateData?$filter=month(NEW_DATE)%20eq%2012%20and%20year(NEW_DATE)%20eq%202020"
#' result <- yieldCurve("https://data.treasury.gov/feed.svc/DailyTreasuryRealYieldCurveRateData",
#'             IndexName = "NEW_DATE")
#' result
#'
#' # (5) Daily Treasury Real Long-Term Rates
#' #
#' # Since 2000
#' # Long Term Real Rate Average: The Long-Term Real Rate Average is the
#' # unweighted average of bid real yields on all outstanding TIPS
#' # with remaing maturities of more than 10 years and is intended as a
#' # proxy for long-term real rates.
#' # https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=reallongtermrate
#' # https://data.treasury.gov/feed.svc/DailyTreasuryRealLongTermRateAverageData?$filter=month(QUOTE_DATE)%20eq%2012%20and%20year(QUOTE_DATE)%20eq%202020"
#' result <- yieldCurve("https://data.treasury.gov/feed.svc/DailyTreasuryRealLongTermRateAverageData",
#'             IndexName = "QUOTE_DATE")
#' result
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools Year Month
#' @importFrom fst write.fst read.fst
#' @importFrom data.table rbindlist
#' @importFrom xts as.xts xtsAttributes `xtsAttributes<-`
#' @export
yieldCurve <- function (base = "http://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData",
                        year, month, IndexName){
tryCatchLog::tryCatchLog({

  oldtz <- Sys.getenv("TZ")
  if(oldtz!="UTC") {
    Sys.setenv(TZ="UTC")
  }

  # determine temp folder
  ops <- options()
  if(!"econModel.yieldCurve.folder" %in% Names(ops)) {
    options(append(ops, list(econModel.yieldCurve.folder = paste0(normalizePath(tempdir(), winslash = "/"), "/", "econModel.yieldCurve.folder"))))
  }

  # verify/create temp folder
  if(!dir.exists(getOption("econModel.yieldCurve.folder"))) {
    dir.create(getOption("econModel.yieldCurve.folder"))
  }

  if(missing(year)) {
    year <- DescTools::Year(Sys.Date())
  }
  if(missing(month)){
    month <- DescTools::Month((Sys.Date()))
  }
  if(missing(IndexName)){
    IndexName <- "NEW_DATE"
  }

  location <- base
  yloc <- mloc <- doc <- NULL
  yloc <- if (length(year))
    paste0("year(", IndexName, ")%20eq%20", year)
  mloc <- if (length(month))
    paste0("month(", IndexName, ")%20eq%20", month)
  parameters <- ""
  if (length(yloc) && length(mloc)) {
    parameters = paste0("?$filter=", mloc, "%20and%20", yloc)
  }
  else {
    if (length(yloc))
      parameters = paste("?$filter=", yloc, sep = "")
    if (length(mloc))
      parameters = paste("?$filter=", mloc, sep = "")
  }
  URL <- paste(location, parameters, sep = "")

  DestFileRootName <- paste0(last(strsplit(URL, "(/|\\?)")[[1]],2), collapse =  "$")
  InternetFile <- paste0(normalizePath(getOption("econModel.yieldCurve.folder"), winslash = "/"), "/",  DestFileRootName, ".", "Internet")

  if(!file.exists(paste0(InternetFile, ".fst"))) {
    # query and write

    message(paste0("Downloading . . . ", URL))

    # DEC 2020
    # Warning message:
    # In readLines(con) :
    #   incomplete final line found on 'http://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData'
    # "URL encoding" (URLencode) already done by the original Author
    Page <- suppressWarnings(fetchInternet(URL, Collapse = "", encode = F))

    fst::write.fst(data.frame(cbind(Page)), path = paste0(InternetFile, ".fst"), compress = 100L)

    message("Download complete.")


  } else {

    # read
    Page <- fst::read.fst(paste0(InternetFile, ".fst"))$Page

  }

  ConvertedFile <- paste0(InternetFile, ".ConvertedFile")


  if(!file.exists(paste0(ConvertedFile, ".fst"))) {

    message("Converting XML, via PCRE2 regular expressions, to a data.frame . . .")

    VectorOfSubPageCoords <- gregexpr("<m:properties>.*?</m:properties>", Page)

    # 12 seconds # default # (7747 records) # DEC 2020
    ListOfRecords <- mapply(function(SubPageCoords, SubPageCoordsAttrMatchLength) {
      SubPage <- substr(Page, SubPageCoords, SubPageCoords + SubPageCoordsAttrMatchLength - 1L)
      # replace 2 whitespaces with nothing
      SubPage <- gsub("\\s{2,}", "" , SubPage)
      SubPage <- sub("<m:properties>", "", SubPage)
      SubPage <- sub("<\\/m:properties>", "", SubPage)
      # formatted
      SubPage <- gsub("<d:", "\n<d:", SubPage)
      # top - remove that CR
      SubPage <- gsub("^\\s*", "", SubPage)
      VectorOfRecordElements <- strsplit(SubPage, "\n")[[1]]
      # Preceeded by regex "^<d:"
      # PCRE2 Lookarounds require "perl = TRUE"
      # PCRE2 [A-Za-z_]+ incorrectly
      #   breaks after the first found underscore(_)
      ColNamesCoordinates <- regexpr("(?<=^<d:)\\w+", VectorOfRecordElements, perl = TRUE)
      ColNames <-  substr(VectorOfRecordElements, ColNamesCoordinates, ColNamesCoordinates + attr(ColNamesCoordinates, "match.length") - 1L)
      ColTypesCoordinates <- regexpr("(?<=m:type=\"Edm.)\\w+", VectorOfRecordElements, perl = TRUE)
      ColTypes <-  substr(VectorOfRecordElements, ColTypesCoordinates, ColTypesCoordinates + attr(ColTypesCoordinates, "match.length") - 1L)
      ColTypes <- sapply(ColTypes, function(x) {
        if(nchar(x)) {
          return(x)
        } else {
          return("Character")
        }
      }, USE.NAMES = F)

      # Specific case in "DailyTreasuryLongTermRateData"
      # incorrectly un-Typed at the source . . . No.
      # EXTRAPOLATION_FACTOR looks (obviously) like a Double
      # but is is "not a Double". Commonly is has the value of "N/A".
      # NO!
      # ColTypes[ColNames %in% "EXTRAPOLATION_FACTOR"] <- "Double"

      # anything between ">" and "<"
      # CAN be stored a zero length string ""
      # If this does not find, then the result value becomes NA_character_
      #   see below: "Character (special cases)"
      ColValuesCoordinates <- regexpr("(?<=>).*(?=<)", VectorOfRecordElements, perl = TRUE)
      #
      ColValues <-  substr(VectorOfRecordElements, ColValuesCoordinates, ColValuesCoordinates + attr(ColValuesCoordinates, "match.length") - 1L)

      Record <- data.frame(mapply(function(ColName, ColType, ColValue) {

        if(ColType == "Int32") {
          if(nchar(ColValue)) {
            return(as.integer(ColValue))
          } else {
            return(NA_integer_)
          }
        }

        if(ColType == "Double") {
          if(nchar(ColValue)) {
            return(as.numeric(ColValue))
          } else {
            return(NA_real_)
          }
        }

        if(ColType == "DateTime") {
          if(nchar(ColValue)) {
            return(as.POSIXct(ColValue))
          } else {
            return(NA_real_)
          }
        }

        # ColType == "Character"(default)

        # Character (special cases)
        if(length(ColValue) & !nchar(ColValue)) {
          # empty string ""
          return(ColValue)
        }

        # Character (special cases)
        if (!length(ColValue)){
          # NULL or "unknown" or "everything else"
          return(NA_character_)
        }

        # Not converting column names like "_DATE$"
        # that are of type "Character"
        # that contain "YYYY/MM/DD".
        #
        # The end user can do that.

        # everything else
        return(ColValue)

      }, ColNames, ColTypes, ColValues, SIMPLIFY = F))
      Record

    }, VectorOfSubPageCoords[[1]], attr(VectorOfSubPageCoords[[1]], "match.length"), SIMPLIFY = F)

    message("Converting complete.")

    message("Row bind conversion beginning.")
    # Read docs carefully: "fill = T"
    # ? data.table::rbindlist
    # instantaneous # default # (7747 records) # DEC 2020
    Table <- data.table::rbindlist(ListOfRecords, fill = T)
    message("Row bind conversion complete.")

    fst::write.fst(Table, path = paste0(ConvertedFile, ".fst"), compress = 100)

  } else {

    Table <- fst::read.fst(path = paste0(ConvertedFile, ".fst"), as.data.table = TRUE)

  }

  # choose the column of the first DATE in
  # the/a *_DATE and is the 2nd column (after the 1st column that is the *Id)
  areColNamesDATE <- !is.na(match(grepl("^.*_DATE$", colnames(Table)), TRUE))

  # the first DATE
  te <- rep(FALSE, length(colnames(Table)))
  te[first(seq_along(colnames(Table))[areColNamesDATE])] <- TRUE
  isColName1stDATE <- te
  #
  OrigDATEName <- colnames(Table)[isColName1stDATE]

  # rename
  colnames(Table)[isColName1stDATE] <- "DATE"

  # col info
  areColumnsNumeric <- sapply(Table, function(x) inherits(x, "numeric"))
  areColNamesFACTOR <- !is.na(match(grepl("^.*_TYPE$", colnames(Table)), TRUE))
  areColNamesNumericOrFACTOR <- areColumnsNumeric | areColNamesFACTOR
  ColNamesNumericOrFACTOR <- colnames(Table)[areColNamesNumericOrFACTOR]

  # so I can use data.frame assessors
  Table <- data.frame(Table)
  #
  # it has a factor, so I cast
  if(any(areColNamesFACTOR)) {
    message("xcast beginning . . .")
    Xts <- xcast(Table[,c("DATE",ColNamesNumericOrFACTOR), drop = F], IndexVar = "DATE", ValueVar = "RATE")
    # xts object is out
    # NOT USED: Instead returning all of the data
    OtherColumns <- colnames(Table)[!colnames(Table) %in% c("DATE", colnames(Table)[ColNamesNumericOrFACTOR])]
    # anything else is returned in xts user attributes
    # let the user decide what to do with those
    xts::xtsAttributes(Xts) <- list(IndexName = OrigDATEName)
    # because of the "cast" this data is worthless unless all fo the data is returned.
    # Approx: only the original "*_DATE$" column has had its name change to "DATE"
    xts::xtsAttributes(Xts) <- list(ApproxDataSheet = Table)
    message("xcast complete.")
  } else {
    NumericTable    <- Table[,  areColumnsNumeric, drop = F]
    NonNumericTable <- Table[, !areColumnsNumeric, drop = F]
    # move the index to the rownames (data.table does not do rownames)
    # side note: data.table does not do rownames (directly)
    # DATE is not numeric!
    rownames(NumericTable) <- NonNumericTable[["DATE"]]

    Xts <- xts::as.xts(NumericTable)
    # anything else is returned in xts user attributes
    # let the user decide what to do with those

    xts::xtsAttributes(Xts) <- list(IndexName = OrigDATEName)
    # just appends . . .
    # vectors, lists, data.frames "names of Variables" access
    # Approx: only the original "*_DATE$" column has had its name change to "DATE"
    xts::xtsAttributes(Xts) <- list(LimitedApproxDataSheet = NonNumericTable[!colnames(NonNumericTable) %in% "DATE"])
  }

  Sys.setenv(TZ=oldtz)

  Xts
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}






#' Download vintage dates of Federal Reserve Economic Data - ALFRED(R)
#'
#' Downloads vintage dates of the published tail dates of 'Date Range' of the specified Symbol from 'research.stlouisfed.org'.
#'
#' Used internally by the function getSymbols.ALFRED
#'
#' Note, this function does not use the official API.  The official API usage can be performed using the R CRAN package alfred function get_alfred_series.  In some cases, this function vinDates may be faster.
#'
#' @param Symbol specifying the name of the symbol to be loaded
#' @param src see R CRAN package quantmod function getSymbols
#' @return character vector of 'Last Updated' vintage dates
#' @references
#'\cite{Download Data for Gross Domestic Product (GDP)
#'\url{https://alfred.stlouisfed.org/series/downloaddata?seid=GDP}
#'}
#' @references
#'\cite{Download Data Help
#'\url{https://alfred.stlouisfed.org/help/downloaddata}
#'}
#' @examples
#' \dontrun{
#' # Smoothed U.S. Recession Probabilities
#' # Source: Piger, Jeremy Max, Chauvet, Marcelle
#' # https://fred.stlouisfed.org/data/RECPROUSM156N.txt
#' #
#' vinDates("RECPROUSM156N")
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom curl curl_version new_handle handle_setopt curl handle_reset
vinDates <- function(Symbol, src = "ALFRED") {
tryCatchLog::tryCatchLog({

  if(length(Symbol) && !class(Symbol) %in% "character") {
    stop("Symbol must be of class \"character\".")
  } else if(!length(Symbol)){
    stop("Symbol can not be NULL")
  }

  if(length(src) && !class(src) %in% "character") {
    stop("src must be of class \"character\".")
  } else if(length(src) && !src %in% "ALFRED") {
    stop("src must be \"ALFRED\". Default is \"ALFRED\".")
  } else if(!length(src)){
    stop("src can not be NULL")
  }

  if(src == "ALFRED") {
    URL <- paste0("https://alfred.stlouisfed.org/series/downloaddata?seid=", Symbol)
    # # scrape
    # h <- curl::new_handle()
    # useragent <- paste("curl/", curl::curl_version()$version, " function vinDates of R CRAN package econModel calling function curl of R CRAN package curl", sep = "")
    # # debug in Fiddler 4
    # # curl --proxy 127.0.0.1:8888 --insecure -A "custom agent" https://alfred.stlouisfed.org/series/downloaddata?seid=GDP
    # # Body dropped from POST request when using proxy with NTLM authentication #146
    # # https://github.com/jeroen/curl/issues/146
    # # curl::handle_setopt(h, .list = list(proxy = "127.0.0.1", proxyport = 8888, useragent = useragent))
    # curl::handle_setopt(h, .list = list(useragent = useragent))
    # # go for it
    # # Page <- paste0(scan(URL, what = character()), collapse = " ")
    # con <- curl::curl(URL, handle = h)
    # Page <- paste0(readLines(con), collapse = " ")
    # # docs say that it does not do much
    # curl::handle_reset(h)
    # close(con)

    Page <- fetchInternet(URL, Collapse = " ")

    # coords of area of interest and non-greedy regex (.*?)
    SubPageCoords <- regexpr("<select id=\"form_selected_vintage_dates\".*?</option></select>", Page)
    # area
    SubPage <- substr(Page, SubPageCoords, SubPageCoords + attr(SubPageCoords, "match.length") - 1L)
    # date coords
    LastUpdatedCoords <- gregexpr("\\d{4}-\\d{2}-\\d{2}", SubPage)
    # dates
    LastUpdatedDates  <- sort(unique(sapply(LastUpdatedCoords[[1]], function(x) { substr(SubPage, x, x + 9) })))
  }
  # oldest vintage to youngest vintage
  return(LastUpdatedDates)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' @rdname getSymbols.ALFRED
#' @export
#' @importFrom tryCatchLog tryCatchLog
getFRED <- function(Symbols,
                    env = parent.frame(),
                    return.class = "xts",
                    returnIndex = "ObservationDate",
                    VintageId = "Latest",
                    nameVintagedId = F,
                    EarliestLastUpdDate = NULL,
                    LookBack = "Beginning",
                    VintagesPerQuery = 12,
                    FullOldestVintageData = F,
                    DataSheet = F,
                    allowParallel = F,
                    MaxParallel = NULL,
                    ...) {
tryCatchLog::tryCatchLog({

  getALFRED(Symbols = Symbols,
             env = env,
             return.class = return.class,
             returnIndex = returnIndex,
             VintageId = VintageId,
             nameVintagedId = nameVintagedId,
             EarliestLastUpdDate = EarliestLastUpdDate,
             LookBack = LookBack,
             VintagesPerQuery = VintagesPerQuery,
             FullOldestVintageData = FullOldestVintageData,
             DataSheet = DataSheet,
             allowParallel = allowParallel,
             MaxParallel = MaxParallel,
             ...)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' @examples
#' \dontrun{
#'
#' # Smoothed U.S. Recession Probabilities (RECPROUSM156N)
#' # Source: Piger, Jeremy Max, Chauvet, Marcelle
#' # https://fred.stlouisfed.org/series/RECPROUSM156N
#' # and
#' # https://fred.stlouisfed.org/data/RECPROUSM156N.txt
#'
#' library(quantmod)
#'
#' getSymbols("RECPROUSM156N", src =   "FRED")
#' [1] "RECPROUSM156N"
#'
#' # src = "ALFRED"
#' getALFRED("RECPROUSM156N", LookBack = 4)
#'
#' Processing vintages: 2012-09-04 ... 2013-08-08 of RECPROUSM156N.vin
#' Processing vintages: 2013-09-03 ... 2014-08-01 of RECPROUSM156N.vin
#' Processing vintages: 2014-09-01 ... 2015-08-06 of RECPROUSM156N.vin
#' Processing vintages: 2015-09-01 ... 2016-08-05 of RECPROUSM156N.vin
#' Processing vintages: 2016-09-01 ... 2017-08-03 of RECPROUSM156N.vin
#' Processing vintages: 2017-09-01 ... 2018-08-01 of RECPROUSM156N.vin
#' Processing vintages: 2018-09-03 ... 2019-09-03 of RECPROUSM156N.vin
#' Processing vintages: 2019-10-01 ... 2020-09-01 of RECPROUSM156N.vin
#' Processing vintages: 2020-10-01 ... 2020-10-01 of RECPROUSM156N.vin
#' [1] "RECPROUSM156N.vin"
#' }
#' @rdname getSymbols.ALFRED
#' @export
#' @importFrom tryCatchLog tryCatchLog
getALFRED <- function(Symbols,
                      env = parent.frame(),
                      return.class = "xts",
                      returnIndex = "ObservationDate",
                      VintageId = NULL,
                      nameVintagedId = F,
                      EarliestLastUpdDate = NULL,
                      LookBack = 3,
                      VintagesPerQuery = 12,
                      FullOldestVintageData = F,
                      DataSheet = F,
                      allowParallel = F,
                      MaxParallel = NULL,
                      ...) {
tryCatchLog::tryCatchLog({

  getSymbols(Symbols = Symbols,
             src = "ALFRED",
             env = env,
             return.class = return.class,
             returnIndex = returnIndex,
             VintageId = VintageId,
             nameVintagedId = nameVintagedId,
             EarliestLastUpdDate = EarliestLastUpdDate,
             LookBack = LookBack,
             VintagesPerQuery = VintagesPerQuery,
             FullOldestVintageData = FullOldestVintageData,
             DataSheet = DataSheet,
             allowParallel = allowParallel,
             MaxParallel = MaxParallel,
             ...)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}




#' Download Federal Reserve Economic Data - ALFRED(R)
#'
#' R access to the latest observation(s) of the vintages of over seven hundred thousand data series accessible via the St. Louis Federal Reserve Bank's ALFRED (Archival FRED:  Federal Reserve Bank of St. Louis's _Archiva_L  _Federal _Reserve _Economic _Data) system \url{https://alfred.stlouisfed.org/}; collects and displays data from the ALFRED vintages that (as seen by a public user during a zone of time at FRED), 'per observation date', are the 1st appearance of an observation date (and its datum).
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
#' @param VintageId download one specific vintage. User input is expected to be a vector of one. The vintage can be the form of a character or Date.  Default is NULL meaning try to download all vintages that have not been restricted elsewhere.  To otherwise restrict by range, see the parameter EarliestLastUpdDate.  To see the available vintage dates, use the function vinDates.  The parameter value can also be "Latest".
#' @param nameVintagedId add the VintageId (or the most recent "Last Updated" date VintageId) to the name of what is returned
#' @param version Integer. Default is 1L.  Version of data ever to be assigned to that point in time: version = 1L means "original (first(1st)) version"; version = 2L means "first(1st) revision"; version = 3L "second(2nd) revision", and so on.
#' @param returnIndex one of "ObservationDate" (row element date) or "LastUpdatedDate" (vintage date). Default is ObservationDate".  Note, in FRED and ALFRED an 'observation date'(row element date) is  not the 'date of measurement'. The 'observation date' (typically) is (observes) the beginning of the 'date range' (its period: ObservationDate + Frequency).  The LastUpdatedDate date, that is, the vintage date of publication, is after the the period has completed, that is after  ObservationDate + Frequency.  See DATE(observation date a.k.a row element date), Frequency, Date Range, and 'Last Updated' in  in \url{https://fred.stlouisfed.org/data/RECPROUSM156N.txt}
#' @param EarliestLastUpdDate character or Date.  Earliest date that is before or 'at' the vintage 'Last Updated' date in the past that a user may wish to query upon. Default is NULL (no restriction).  This is useful in the situation when the user already owns prior data, and just wants just some recent data.  Internally, this just subtracts off some 'Last Updated' dates from the results of calling the function vinDates (xor vintages that have been entered by the user throught the paramter VintageId).  Note, if this paramter EarliestLastUpdDate, is used, the tail the returned data (older data) is not expected to be correct.  The reason is that, not all vintages can bee seen, so the clause is no longer true: "the first available datam per specific date of all vintages".
#' @param LookBack how deep in periods to look back for the latest observation in all of the non-oldest vintages.  Meant to use with datasets with a wide range of time between the Measurement interval and the Validity interval.  From the 'Last Updated' date try to peek back in time to the 1st vintage with a published tail 'Date Range' date that is within variable 'LookBack' periods. If the periodicy is "day" and, just after a three(3) day holiday weekend, to reach back from a Tuesday to a Friday, parameter LookBack is increased to a minimum value of 4.  Default is 3. Increase this value if much time exists between the tail date of 'Date Range' and the 'Last Updated' date: meaning zero(0) observations exist in the LookBack period.  The R CRAN package xts function periodicity determines the period of time.  This function is meant to minimize server-side CPU and disk I/O.  . Value can be "Beginning". "Beginning" means lookback to the start.
#' @param VintagesPerQuery number of vintages per HTTPS GET. A.k.a the number of vintages per sheet.   Default is 12.  Common maximum is 12. Value can be "Max". Practical experience has performed with 192.  The maximum may be different during different with not-a-known reason.  This parameter exists to enhance performance by limiting the number of trips to the server.  This parameter is sometimes (but not often) better than the parameter allowParallel. On many occasions  when using this parameter with values greater than 12, the requested data is missing from the returned data set.
#' @param FullOldestVintageData if TRUE, then also return the oldest vintage data and keep(prepend) its data.  Default is FALSE. Useful when 'as much data as possible' is important.
#' @param DataSheet if TRUE, then also return all of the vintages in an xts attribute 'DataSheet'. Default is FALSE.  Useful for debugging.  Useful as a tool of doing more (future) coding or user-end research.
#' @param allowParallel if TRUE, then collect groups of 'sheets of VintagesPerQuery vintages' in parallel.  Default is FALSE.  (Improved) performance will vary: this is more useful on (more data points) weekly data or daily data. Because this is a server side activity, the number of parallel processes does NOT depend on the local machine CPUs.
#' @param MaxParallel if allowParallel is TRUE, then set the maximum number of parallel processes. Default is NULL (no limit).  If this parameter is NULL, then the approximate maximum number of parallel processes is 'unique(ceiling(seq_along(vinDates(SYMBOL)/VintagesPerQuery)))' where the vector from vinDates(SYMBOL) may be reduced by limiting data using EarliestLastUpdDate. Good choices of this parameter may depend on, the amount of the client host hardware CPU and memory.
#' @param ... additional parameters
#' @return as R CRAN package quantmod function getSymbols. See the parameter returnIndex for a user-choice the xts objects returned index
#'
#' @author Andre Mikulec   (adapted from the original code)
#' @author Jeffrey A. Ryan (original code that is inside the R CRAN package quantmod function getSymbols.FRED)
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
#' getSymbols("RECPROUSM156N", src = "ALFRED", LookBack = 4)
#'
#' Processing vintages: 2012-09-04 ... 2013-08-08 of RECPROUSM156N.vin
#' Processing vintages: 2013-09-03 ... 2014-08-01 of RECPROUSM156N.vin
#' Processing vintages: 2014-09-01 ... 2015-08-06 of RECPROUSM156N.vin
#' Processing vintages: 2015-09-01 ... 2016-08-05 of RECPROUSM156N.vin
#' Processing vintages: 2016-09-01 ... 2017-08-03 of RECPROUSM156N.vin
#' Processing vintages: 2017-09-01 ... 2018-08-01 of RECPROUSM156N.vin
#' Processing vintages: 2018-09-03 ... 2019-09-03 of RECPROUSM156N.vin
#' Processing vintages: 2019-10-01 ... 2020-09-01 of RECPROUSM156N.vin
#' Processing vintages: 2020-10-01 ... 2020-10-01 of RECPROUSM156N.vin
#' [1] "RECPROUSM156N.vin"
#'
#' # Note, the distance between the graphic date and the 'Last Updated' date
#' # is about two(2) months, so one may shift the graphic to the right
#' # by two(2) months to get the real story.
#' #
#' # See: 'Date Range' and
#' 'Last Updated' in https://fred.stlouisfed.org/data/RECPROUSM156N.txt
#' # See: vinDates("RECPROUSM156N")
#' #
#' # rough way to get the real story based on 'Date Range' and "Last Updated"
#' # after the last observation date the "Last Updated" published date is two(2) months later
#' # the returnIndex parameter (default) is "ObservationDate (row/element date)
#' index(RECPROUSM156N) <- index(RECPROUSM156N) + 61
#' index(RECPROUSM156N.vin) <- index(RECPROUSM156N.vin) + 61
#' dygraphs::dygraph(merge(RECPROUSM156N, RECPROUSM156N.vin))
#'
#' # better way to get the real story
#' # instead make observation dates to be the "Last Updated" published date.
#' # the returnIndex parameter is "LastUpdatedDate" (vintage published date)
#' getSymbols("RECPROUSM156N", src = "ALFRED", returnIndex = "LastUpdatedDate", LookBack = 4)
#' dygraphs::dygraph(merge(RECPROUSM156N, RECPROUSM156N.vin))
#'
#' # get the most recent vintage (most recent VintageId)
#' getALFRED("RECPROUSM156N", VintageId = "Latest", LookBack = "Beginning", verbose = T)
#' # same
#' getFRED("RECPROUSM156N", verbose = T)
#' downloading  RECPROUSM156N .....
#'
#' Processing vintages: 2020-10-01 ... 2020-10-01 of RECPROUSM156N.vin
#'
#' # get just this exact vintage data and all of its data
#' # To get all of the data the user chooses look back 100 years)
#' # (This series has known periods of one month long in duration)
#' getSymbols("RECPROUSM156N", src = "ALFRED", VintageId = "2020-01-02", LookBack = 1200)
#'
#' # get this exact vintage and all of its data
#' getSymbols("RECPROUSM156N", src = "ALFRED", VintageId = "2020-01-02",
#'             LookBack = "Beginning")
#'
#' # get just this exact vintage and its most recent data (some of its data)
#' # that is restricted by the default short time Lookback
#' getSymbols("RECPROUSM156N", src = "ALFRED", VintageId = "2020-01-02")
#'
#' # same as above, and include the VintageId in the column name
#' getSymbols("RECPROUSM156N", src = "ALFRED", VintageId = "2020-01-02", nameVintagedId = T)
#' # Processing vintages: 2020-01-02 ... 2020-01-02 of RECPROUSM156N.vin.2020.01.02
#' # [1] "RECPROUSM156N.vin.2020.01.02"
#' RECPROUSM156N.vin.2020.01.02
#' #            RECPROUSM156N.vin.2020.01.02
#' # 2019-10-01                         1.82
#' # 2019-11-01                         0.60
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
#' #     MM is the month in number values of 01 through 12
#' #       representing January through December.
#' #     DD is the daty of the month from 01 through the last day of that month.
#' colnames(data.frame(xtsAttributes(RECPROUSM156N.vin)$DataSheet))
#'
#' # From (above) https://fred.stlouisfed.org/data/RECPROUSM156N.txt
#' #    one knows that Frequency is 'Monthly'.
#' # To get  all of the data (in all of the vintages), set the
#' #   LookBack to be a 'high value' e.g. perhaps, 100 years
#' #   (1200: 12 months x 100 years)
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
#' # the user does not want to query upon vintages before vintage at the
#' # 'Last Updated' date of "2020-01-01"
#' # Note, if this parameter is used, the tail the returned data (older data)
#' # is not expected to be correct.  The reason is that, not all vintages can bee seen,
#' # so the clause is no longer true:
#' # "the first available datam per specific date of all vintages".
#' getSymbols("EFFR", src = "ALFRED", EarliestLastUpdDate = "2020-01-01")
#' # better (just recent data)
#' getSymbols("EFFR", src = "ALFRED", EarliestLastUpdDate = Sys.Date() - 35)
#'
#' # first(1st) revision (and its last updated date (publication date))
#' getSymbols("GDP", src = "ALFRED", LookBack = 5, DataSheet = T, version = 2, returnIndex = "LastUpdatedDate")
#' head(GDP.vin,7)
#' This is correct. Four(4) first revisions (version = 2)
#' (but for four different observation dates (not shown))
#' were published on 1991-12-20.
#' GDP.vin
#' 1991-12-20  5570.5 # correct
#' 1991-12-20  5557.5 # correct
#' 1991-12-20  5589.0 # correct
#' 1991-12-20  5652.6 # correct
#' 1991-12-20  5709.2
#' 1992-02-28  5746.7
#' 1992-05-29  5817.5
#'
#' # get multiple Symbols in one user execution
#' # using R CRAN package quantmod function getSymbols
#' getSymbols("RECPROUSM156N;GDP", src = "ALFRED", EarliestLastUpdDate = "2020-01-01",
#'            nameVintagedId = T)
#' # only the last Symbol is printed back to the console
#' # Moreover, both series are actually there.
#'
#' # get multiple Symbols in one user execution
#' mapply(
#'   function(Symbol, VintageId, LookBack) {
#'     getSymbols(Symbol, src = "ALFRED", VintageId = VintageId,
#'                        nameVintagedId = T,
#'                        LookBack = LookBack, env = .GlobalEnv)
#'   },
#'   c("RECPROUSM156N",  "RECPROUSM156N", "GDP"),           # Symbol
#'   c("2020-01-02",     "2019-01-02",    "2019-12-20"),    # VintageId
#'   c(18,                12,              6)               # LookBack # months and quarters
#' )
#' Processing vintages: 2020-01-02 ... 2020-01-02 of RECPROUSM156N.vin.2020.01.02
#' Processing vintages: 2019-01-02 ... 2019-01-02 of RECPROUSM156N.vin.2019.01.02
#' Processing vintages: 2019-12-20 ... 2019-12-20 of GDP.vin.2019.12.20
#'                  RECPROUSM156N                  RECPROUSM156N                   GDP
#' "RECPROUSM156N.vin.2020.01.02" "RECPROUSM156N.vin.2019.01.02"  "GDP.vin.2019.12.20"
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom foreach foreach `%do%` `%dopar%`
#' @importFrom doParallel registerDoParallel  stopImplicitCluster
#' @importFrom utils tail read.csv
#' @importFrom stats na.omit
#' @importFrom methods hasArg
#' @importFrom curl curl_version new_handle handle_setopt curl handle_reset
#' @importFrom zoo as.Date as.yearmon as.yearqtr na.trim coredata index `index<-`
#' @importFrom xts xts as.xts last periodicity
#' @importFrom xts tclass `tclass<-` tformat `tformat<-` tzone `tzone<-`  xtsAttributes `xtsAttributes<-`
#' @importFrom xts as.xts last
#' @importFrom quantmod importDefaults getSymbols getSymbolLookup
getSymbols.ALFRED <- function(Symbols,
                              env,
                              return.class = "xts",
                              returnIndex = "ObservationDate",
                              VintageId = NULL,
                              nameVintagedId = F,
                              version = 1L,
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
  # NO
  # assign("oldtz", oldtz, envir = environment())

  quantmod::importDefaults("getSymbols.ALFRED")

  this.env <- environment()
  for (var in names(list(...))) {
    assign(var, list(...)[[var]], this.env)
  }
  if (!methods::hasArg("verbose"))
    verbose <- FALSE
  if (!methods::hasArg("auto.assign"))
    auto.assign <- TRUE

  if(length(returnIndex) && !returnIndex %in% c("ObservationDate", "LastUpdatedDate")) {
    stop("returnIndex must be just one of \"ObservationDate\" or \"LastUpdatedDate\"")
  } else if(!length(returnIndex)) {
    stop("returnIndex can not be NULL")
  }

  if(length(VintageId) && !class(VintageId) %in% c("Date", "character")) {
    stop("VintageId must be NULL or of class \"Date\" or \"character\"")
  } else if(length(VintageId) && VintageId == "Latest") { # good, so skip
  } else if(length(VintageId) && class(try( {zoo::as.Date(VintageId)}, silent = F)) == "try-error") {
    stop("VintageId must be NULL, convertible to a Date-like, or \"Latest\"")
  } else if(1L < length(VintageId)) {
    stop("VintageId must be just one element")
  }

  if(length(nameVintagedId) &&!class(nameVintagedId) %in% c("logical")) {
    stop("nameVintagedId must be of class \"logical\"")
  } else if(!length(nameVintagedId)) {
    stop("nameVintagedId can not be NULL")
  }

  if(length(version) && !class(version) %in% c("numeric", "integer")) {
    stop("version must be of class \"numeric\" or \"integer\"")
  } else if(!length(version)) {
    stop("version can not be NULL")
  } else if(length(version) && class(version) %in% c("numeric")) {
    if(version < 1) {
      stop("version must be of value 1 or greather")
    }
  }

  if(length(EarliestLastUpdDate) && !class(EarliestLastUpdDate) %in% c("Date", "character")) {
    stop("EarliestLastUpdDate must be NULL or of class \"Date\" or \"character\"")
  } else if(length(EarliestLastUpdDate) && class(try( {zoo::as.Date(EarliestLastUpdDate)}, silent = F)) == "try-error") {
    stop("EarliestLastUpdDate must be NULL or convertible to a Date-like")
  }

  if(length(LookBack) && !class(LookBack) %in% c("numeric", "integer", "character")) {
    stop("LookBack must be of class \"numeric\" or \"integer\" or \"character\"")
  } else if(class(LookBack) %in% c("character") && LookBack != "Beginning") {
    stop("LookBack if a character must be of value \"Beginning\"")
  } else if(!class(LookBack) %in% c("character") && length(LookBack) && LookBack < 1) {
    stop("LookBack must be of value 1 or greater")
  } else if(!length(LookBack)) {
    stop("LookBack can not be NULL")
  }
  if(is.numeric(LookBack)) LookBack <- floor(LookBack)

  if(length(VintagesPerQuery) && !class(VintagesPerQuery) %in% c("numeric", "integer")) {
    stop("VintagesPerQuery must be of class \"numeric\" or \"integer\"")
  } else if(length(VintagesPerQuery) && VintagesPerQuery < 1) {
    stop("VintagesPerQuery must be of value 1 or greater")
  } else if(!length(VintagesPerQuery)) {
    stop("VintagesPerQuery can not be NULL")
  }
  VintagesPerQuery <- floor(VintagesPerQuery)

  if(length(FullOldestVintageData) &&!class(FullOldestVintageData) %in% c("logical")) {
    stop("FullOldestVintageData must be of class \"logical\"")
  } else if(!length(FullOldestVintageData)) {
    stop("FullOldestVintageData can not be NULL")
  }

  if(length(DataSheet) && !class(DataSheet) %in% c("logical")) {
    stop("DataSheet must be of class \"logical\"")
  } else if(!length(DataSheet)) {
    stop("DataSheet can not be NULL")
  }

  if(length(allowParallel) && !class(allowParallel) %in% c("logical")) {
    stop("allowParallel must be of class \"logical\"")
  } else if(!length(allowParallel)) {
    stop("allowParallel can not be NULL")
  }

  if(length(MaxParallel) && !class(MaxParallel) %in% c("numeric", "integer")) {
    stop("MaxParallel must be NULL or of class \"numeric\" or \"integer\"")
  } else if(length(MaxParallel) && MaxParallel < 1) {
    stop("MaxParallel must be NULL or of value 1 or greater")
  }
  if(length(MaxParallel)) MaxParallel <- floor(MaxParallel)

  default.env <- env
  default.return.class <- return.class
  default.returnIndex <- returnIndex
  default.VintageId <- VintageId
  default.nameVintagedId <- nameVintagedId
  default.version = version
  default.EarliestLastUpdDate <- EarliestLastUpdDate
  default.LookBack <- LookBack
  default.VintagesPerQuery <- VintagesPerQuery
  default.FullOldestVintageData <- FullOldestVintageData
  default.DataSheet <- DataSheet
  default.allowParallel <- allowParallel
  default.MaxParallel <- MaxParallel

  ANYFRED.URL <- "https://alfred.stlouisfed.org/graph/alfredgraph.csv"
  returnSym <- Symbols
  noDataSym <- NULL
  for (i in seq_along(Symbols)) {
    if (verbose)
      cat("downloading ", Symbols[[i]], ".....\n\n")

    # # update (1 of 2 places)
    # returnSym <- returnSym[returnSym %in% Symbols] <- paste0(Symbols[[i]], ".vin")
    # MOVED TO BELOW

    te <- quantmod::getSymbolLookup()[[Symbols[[i]]]]$env
    if(!is.null(te)) {
      env <- te
    } else {
      env <- default.env
    }
    te <- quantmod::getSymbolLookup()[[Symbols[[i]]]]$return.class
    if(!is.null(te)) {
      return.class <- te
    } else {
      return.class <- default.return.class
    }
    te <- quantmod::getSymbolLookup()[[Symbols[[i]]]]$returnIndex
    if(!is.null(te)) {
      returnIndex <- te
    } else {
      returnIndex <- default.returnIndex
    }
    te <- quantmod::getSymbolLookup()[[Symbols[[i]]]]$VintageId
    if(!is.null(te)) {
      VintageId <- te
    } else {
      VintageId <- default.VintageId
    }
    te <- quantmod::getSymbolLookup()[[Symbols[[i]]]]$nameVintagedId
    if(!is.null(te)) {
      nameVintagedId <- te
    } else {
      nameVintagedId <- default.nameVintagedId
    }
    te <- quantmod::getSymbolLookup()[[Symbols[[i]]]]$version
    if(!is.null(te)) {
      version <- te
    } else {
      version <- default.version
    }
    te <- quantmod::getSymbolLookup()[[Symbols[[i]]]]$EarliestLastUpdDate
    if(!is.null(te)) {
      EarliestLastUpdDate <- te
    } else {
      EarliestLastUpdDate <- default.EarliestLastUpdDate
    }
    te <- quantmod::getSymbolLookup()[[Symbols[[i]]]]$LookBack
    if(!is.null(te)) {
      LookBack <- te
    } else {
      LookBack <- default.LookBack
    }
    te <- quantmod::getSymbolLookup()[[Symbols[[i]]]]$VintagesPerQuery
    if(!is.null(te)) {
      VintagesPerQuery <- te
    } else {
      VintagesPerQuery <- default.VintagesPerQuery
    }
    te <- quantmod::getSymbolLookup()[[Symbols[[i]]]]$FullOldestVintageData
    if(!is.null(te)) {
      FullOldestVintageData <- te
    } else {
      FullOldestVintageData <- default.FullOldestVintageData
    }
    te <- quantmod::getSymbolLookup()[[Symbols[[i]]]]$DataSheet
    if(!is.null(te)) {
      DataSheet <- te
    } else {
      DataSheet <- default.DataSheet
    }
    te <- quantmod::getSymbolLookup()[[Symbols[[i]]]]$allowParallel
    if(!is.null(te)) {
      allowParallel <- te
    } else {
      allowParallel <- default.allowParallel
    }
    te <- quantmod::getSymbolLookup()[[Symbols[[i]]]]$MaxParallel
    if(!is.null(te)) {
      MaxParallel <- te
    } else {
      MaxParallel <- default.MaxParallel
    }

    test <- tryCatchLog::tryCatchLog({
    # test <- try({

      # where to get the vintages' 'Last Updated' dates
      if(is.null(VintageId)) {
        AllLastUpdatedDates <- vinDates(Symbols[[i]])
      } else if(VintageId == "Latest") {
        AllLastUpdatedDates <- xts::last(vinDates(Symbols[[i]]))
      } else {
        # instead the user chooses the vintages
        AllLastUpdatedDates <- as.character(zoo::as.Date(VintageId))
      }

      # just subtracts off some older 'Last Updated' dates from the results of
      # calling the function vinDates
      if(!is.null(EarliestLastUpdDate)) {
        AllLastUpdatedDates <- zoo::as.Date(AllLastUpdatedDates)[zoo::as.Date(EarliestLastUpdDate) <= zoo::as.Date(AllLastUpdatedDates)]
      }
      # used when nameVintagedId == T
      MostRecentLastUpdatedDate <- xts::last(AllLastUpdatedDates)

      # update (1 of 2 places)
      if(!nameVintagedId) {
        returnSym <- returnSym[returnSym %in% Symbols] <- paste0(Symbols[[i]], ".vin")
      } else {
        returnSym <- returnSym[returnSym %in% Symbols] <- paste0(Symbols[[i]], ".vin", "." , gsub("-", ".", MostRecentLastUpdatedDate))
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
      Often <- xts::periodicity(zoo::index(quantmod::getSymbols(Symbols[[i]], src = "FRED", auto.assign = FALSE)))$label
      # note, could read the Frequency here: https://fred.stlouisfed.org/data/GDP.txt

      # ALFRED limits 'VintagesPerQuery' (default 12) (groups of 'sheets of 'VintagesPerQuery' vintages')
      # Split a vector into chunks in R
      # https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r
      #
      SplittedLastUpdatedDates <- split(AllLastUpdatedDates, ceiling(seq_along(AllLastUpdatedDates)/VintagesPerQuery))

      if(allowParallel) {
        if(!is.null(MaxParallel)) {
          MaxDoParallelCores <- MaxParallel
        } else {
          MaxDoParallelCores <- length(SplittedLastUpdatedDates)
        }
        doParallel::registerDoParallel(cores = MaxDoParallelCores)
      }


      # OCT 2020
      # devtools::check(manual = TRUE, args = c('--as-cran'))
      # No visible binding for global variable
      # August 18, 2019 by Random R Ramblings
      # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
      # https://nathaneastwood.github.io/2019/08/18/no-visible-binding-for-global-variable/
      LastUpdatedDates <- NULL
      #
      # something similar to what package caret function nominalTrainWorkflow does
      `%op%` <- if(allowParallel) { foreach::`%dopar%` } else { foreach::`%do%` }
      foreach::foreach (LastUpdatedDates = SplittedLastUpdatedDates, .packages = "xts") %op% {

        LengthOfLastUpdatedDates <- length(LastUpdatedDates)

        cat(paste0("Processing vintages: ", LastUpdatedDates[1], " ... ", LastUpdatedDates[LengthOfLastUpdatedDates]), "of", returnSym, "\n")

        # vintages
        URL <- paste(ANYFRED.URL, "?id=",           paste0(rep(Symbols[[i]], LengthOfLastUpdatedDates), collapse = ","), sep = "")
        # vintages last updated dates (validation)
        URL <- paste(       URL, "&vintage_date=", paste0(LastUpdatedDates,                            collapse = ","), sep = "")

        # The time distance between the Measurement interval and Validity interval is
        # not greater than two(three) periods ago (assuming/allowing one revision every so often)
        # so tell the server (and save CPU) to "not try and get past dates that are too old")
        #
        # How to subtract months from a date in R?
        # https://stackoverflow.com/questions/5225823/
        #   how-to-subtract-months-from-a-date-in-r/5226089

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
          # if not a number, expected is "LookBack == "WORD""
          #   so do not bother to set
          if(is.numeric(LookBack)) LookBack <-  max(4, LookBack)
        }

        # ignoring cosd
        if(!LookBack == "Beginning") {

          # do not look back more than three periods ago.  This limits server CPU and disk I/O.
          CoStartDates <- sapply(LastUpdatedDates, function(x) {as.character(seq(as.Date(x), length = 2, by = paste0("-", as.character(LookBack), " ", Often, "s")))[2]})

          # exception to the "do not look back too far" is the oldest Vintage
          if(FullOldestVintageData && OldestVintageDate == names(CoStartDates[1])) {
            # part i of 2
            CoStartDates[1] <- "1776-07-04"
          }
          URL <- paste(URL, "&cosd=", paste0(CoStartDates, collapse = ","), sep = "")
        }

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

        # h <- curl::new_handle()
        # useragent <- paste("curl/", curl::curl_version()$version, " function getSymbols.ALFRED of R CRAN package econModel calling function curl of R CRAN package curl", sep = "")
        # # debug in Fiddler 4
        # # curl --proxy 127.0.0.1:8888 --insecure -A "custom agent" https://alfred.stlouisfed.org/series/downloaddata?seid=GDP
        # # Body dropped from POST request when using proxy with NTLM authentication #146
        # # https://github.com/jeroen/curl/issues/146
        # # curl::handle_setopt(h, .list = list(proxy = "127.0.0.1", proxyport = 8888, useragent = useragent))
        # curl::handle_setopt(h, .list = list(useragent = useragent))
        # # go for it
        # con <- curl::curl(URL, handle = h)

        con <- fetchInternet(URL, conOut = T)

        # can be a readable text-mode connection
        # (which will be opened for reading if necessary,
        # and if so closed (and hence destroyed)
        # at the end of the function call)
        fr <- utils::read.csv(con, na.string = ".")

        ColnamesFR <- colnames(fr)

        if (verbose)
          cat(paste0("Done downloading!\n"))
        fr <- xts::xts(as.matrix(fr[, -1]), as.Date(fr[, 1], origin = "1970-01-01"),
                  src = "ALFRED", updated = Sys.time())

        # part ii of 2
        # Reactionary protection against: TOO MUCH DATA
        # IF NO DATA FOUND, then it RETURNS EVERYTHING (TOO MUCH DATA)

        # ignoring cosd
        if(!LookBack == "Beginning") {

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

        }

        # NOTE: return.class must be able to handle mult-dimensional
        # this removes column names
        dim(fr) <- c(NROW(fr), NCOL(fr))

        # Symbol name is already in the column + _ YYMMDD
        colnames(fr) <- as.character(toupper(ColnamesFR[-1]))

        # S3 call to cbind.xts: FR left of fr will set wrong XtsAttributes
        FR <- xts::xts()
        xts::tclass(FR) <- xts::tclass(fr)
        xts::tformat(FR) <- xts::tformat(fr)
        xts::tzone(FR) <- xts::tzone(fr)

        FR <- fr
        xts::xtsAttributes(FR) <- xts::xtsAttributes(fr)

        FR

      } -> ListFR
      if(allowParallel) doParallel::stopImplicitCluster()

      FR <- do.call(cbind, c(list(), ListFR))
      # FR <- do.call(cbind, c(list(), list(FE) ListFR))

      fr <- FR
      xts::xtsAttributes(fr)$OldestVintage <- OldestVintageDate
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
      # # note "na.locf" is from package zoo
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
      FrMatrixTransposedUpsideDown <- FrMatrixTransposed[rev(seq_along(rownames(FrMatrixTransposed))), , drop = F]
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

      # remove columns that only have NAs seen in EFFR on Holidays
      # if the all-NA columns are not removed then NewCoreData will have problems: "Numeric,0" elements
      # This problem did not happend using na.locf.
      # CLEAN
      NewIndexControl <- apply(FrMatrixTransposedUpsideDown, MARGIN = 2,  function(x) !all(is.na(x)))
      FrMatrixTransposedUpsideDown <- FrMatrixTransposedUpsideDown[, NewIndexControl, drop = F]
      # View(FrMatrixTransposedUpsideDown)
      # of the 'first available datum per specific date' of all vintages,
      #   pull its datum down into a single vector
      #   and format that data as input into package xts function as.xts
      NewCoreData <-
        matrix(
          apply(FrMatrixTransposedUpsideDown, MARGIN = 2, function(x) {
            # xts::last(stats::na.omit(x))),
            # xts::last(zoo::na.trim(x, sides = "right"))
            first(last(zoo::na.trim(x, sides = "right"), n = version), n = 1)
          }),
          dimnames = list(colnames(FrMatrixTransposedUpsideDown), NULL)
        ) # actual working default: ncol = 1L
      # View(NewCoreData)
      # note list colnames is sometimes just for display here (just below).
      #      Colnames MAY SOMETIMES later discarded by "index(fr) <-"
      #
      # Browse[2]> NewCoreData
      # [,1]
      # 2019-04-01 21340.27
      # 2019-07-01 21542.54
      # 2019-10-01 21734.27
      # 2020-01-01 21537.94
      # 2020-04-01 19408.76


      # default
      ObservationDateNewIndex <- zoo::index(fr)[NewIndexControl]

      # NewIndexMatrix: not used in default parameter returnCoreData = "ObservationDate"
      NewIndexMatrix <-
        matrix(
          apply(FrMatrixTransposedUpsideDown, MARGIN = 2, function(x) {
            # x is now a horizontal vector: is.null(dim(x)) == TRUE
            Names(first(last(zoo::na.trim(x, sides = "right"), n = version), n = 1))
          }),
          dimnames = list(colnames(FrMatrixTransposedUpsideDown), NULL)
        )
      # NewIndex <- zoo::as.Date(sapply(strsplit(as.vector(zoo::coredata(NewIndexMatrix)), "_"), function(x) { x[2] } ), tryFormats = "%Y%m%d")
      # already is a vector
      LastUpdatedDateNewIndex <- zoo::as.Date(sapply(strsplit(NewIndexMatrix, "_"), function(x) { x[2] } ), tryFormats = "%Y%m%d")

      # default
      if(returnIndex == "ObservationDate") {
        # (almost) no change
        NewIndex   <-  ObservationDateNewIndex
        OtherIndex <-  list(LastUpdatedDate = LastUpdatedDateNewIndex)
      }

      if(returnIndex == "LastUpdatedDate") {
        NewIndex  <- LastUpdatedDateNewIndex
        OtherIndex <-  list(ObservationDate = ObservationDateNewIndex)
      }

      # not the same size ( so can not do "coredata(fr) <- NewCoreData")
      frNew <- xts::as.xts(NewCoreData)
      # index(frNew) <- index(fr)
      zoo::index(frNew) <- NewIndex
      # xts::xtsAttributes(frNew) <- xts::xtsAttributes(fr)
      xts::xtsAttributes(frNew) <- c(list(), xts::xtsAttributes(fr), OtherIndex)

      fr <- frNew

      # need to just keep the oldest vintage column (useless op if ran 'not the same size')
      fr <- fr[,1]
      # need to rename (the ONE column)
      colnames(fr)[1] <- Symbols[[i]]

      fr <- quantmod___convert.time.series(fr = fr, return.class = return.class)
      Symbols[[i]] <- toupper(gsub("\\^", "", Symbols[[i]]))

      # update (2 of 2 places)
      if(!nameVintagedId) {
        Symbols[[i]]    <- paste0(Symbols[[i]], ".vin")
      } else {
        Symbols[[i]]    <- paste0(Symbols[[i]], ".vin", "." , gsub("-", ".", MostRecentLastUpdatedDate))
      }
      colnames(fr)[1] <- Symbols[[i]]

      # debugging
      if(DataSheet == T) {
        xts::xtsAttributes(fr)$DataSheet <- FR
      }

      if (auto.assign)
        assign(Symbols[[i]], fr, env)
    # }, silent = TRUE)
    }, error = function(e) {Sys.setenv(TZ=oldtz); test <- 0L; class(test) <- "try-error"; test}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))

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
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}




