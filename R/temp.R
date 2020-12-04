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
#' getSymbols(c("UNEMP3", "INDPROD2"), src = "USFedPhilForecasts")
#'
#' # standard deviation (sd) and if any data point is NA then Fun results are all NA).
#' getSymbols("UNEMP3", src = "USFedPhilForecasts", Fun = sd)
#'
#' # all columns in one VERY WIDE xts object
#' getSymbols("USFedPhilForecastsData", src = "USFedPhilForecasts")
#'}
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom readxl read_xls excel_sheets
#' @importFrom RQuantLib adjust
#' @importFrom zoo index as.Date
#' @importFrom xts xts
#' @importFrom data.table rbindlist
#' @importFrom DescTools DoCall
#' @export
getSymbols.USFedPhilForecasts <- function(Symbols, env, return.class = "xts",
                                          Fun = mean, ...) {
tryCatchLog::tryCatchLog({

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

  if (verbose) {
    cat("Reading disk file ", DestFile, ".....\n\n")
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

    # data.frame
    fr <- suppressWarnings(readxl::read_xlsx(path = DestFile, sheet = Sheet,
                                             col_names = T, # get the Excel sheet row 1 column names
                                             col_types = "numeric")) # recycled


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
         return(Fun(cx, ...))
       }
     }))

   }), fill = T)

   fr <- xts::xts(as.matrix(fr), Index,
                  src = "USFedPhilForecasts", updated = Sys.time())

   if (verbose)
     cat(paste0( "Done with ", Sheet,".\n\n"))

   FR <- c(list(), FR, list(fr))

  } # for Sheet in Sheets

  fr <- DescTools::DoCall(cbind, FR)

  fri <- fr # pass-throught on "USFedPhilForecastsData"

  # decompose [if any] into [many] Symbol(s), then return
  for (i in seq_along(Symbols)) {

    if (verbose) {
      cat("Selecting ", Symbols[[i]], ".....\n\n")
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
