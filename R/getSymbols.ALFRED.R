


#' Download vintage dates of Federal Reserve Economic Data - ALFRED(R)
#'
#' Downloads vintage dates of the published tail dates of 'Date Range' of the specified Symbol from 'research.stlouisfed.org'.
#'
#' Used internally by the function getSymbols.ALFRED
#'
#' Note, this function does not use the official API.  The official API usage can be performed using the R CRAN package alfred function get_alfred_series.  In some cases, this function getVintages may be faster.
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
#'
#' library(quantmod)
#'
#' # Smoothed U.S. Recession Probabilities
#' # Source: Piger, Jeremy Max, Chauvet, Marcelle
#' # https://fred.stlouisfed.org/data/RECPROUSM156N.txt
#' #
#' getVintages("RECPROUSM156N")
#' Read 2002 items
#'
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom curl curl_version new_handle handle_setopt curl handle_reset
getVintages <- function(Symbol, src = "ALFRED") {
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
    # scrape
    h <- curl::new_handle()
    useragent <- paste("curl/", curl::curl_version()$version, " function getVintages of R CRAN package econModel calling function curl of R CRAN package curl", sep = "")
    # debug in Fiddler 4
    # curl --proxy 127.0.0.1:8888 --insecure -A "custom agent" https://alfred.stlouisfed.org/series/downloaddata?seid=GDP
    # Body dropped from POST request when using proxy with NTLM authentication #146
    # https://github.com/jeroen/curl/issues/146
    # curl::handle_setopt(h, .list = list(proxy = "127.0.0.1", proxyport = 8888, useragent = useragent))
    curl::handle_setopt(h, .list = list(useragent = useragent))
    # go for it
    # Page <- paste0(scan(URL, what = character()), collapse = " ")
    con <- curl::curl(URL, handle = h)
    Page <- paste0(readLines(con), collapse = " ")
    # docs say that it does not do much
    curl::handle_reset(h)
    close(con)
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

})}




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
#' @param VintageId download one specific vintage. User input is expected to be a vector of one. The vintage can be the form of a character or Date.  Default is NULL meaning try to download all vintages that have not been restricted elsewhere.  To otherwise restrict by range, see the parameter EarliestLastUpdDate.  To see the available vintage dates, use the function getVintages.
#' @param nameVintagedId add the VintageId (or the most recent "Last Updated" date VintageId) to the name of what is returned
#' @param returnIndex one of "ObservationDate" (row element date) or "LastUpdatedDate" (vintage date). Default is ObservationDate".  Note, in FRED and ALFRED an 'observation date'(row element date) is  not the 'date of measurement'. The 'observation date' (typically) is (observes) the beginning of the 'date range' (its period: ObservationDate + Frequency).  The LastUpdatedDate date, that is, the vintage date of publication, is after the the period has completed, that is after  ObservationDate + Frequency.  See DATE(observation date a.k.a row element date), Frequency, Date Range, and 'Last Updated' in  in \url{https://fred.stlouisfed.org/data/RECPROUSM156N.txt}
#' @param EarliestLastUpdDate character or Date.  Earliest date that is before or 'at' the vintage 'Last Updated' date in the past that a user may wish to query upon. Default is NULL (no restriction).  This is useful in the situation when the user already owns prior data, and just wants just some recent data.  Internally, this just subtracts off some 'Last Updated' dates from the results of calling the function getVintages (xor vintages that have been entered by the user throught the paramter VintageId).  Note, if this paramter EarliestLastUpdDate, is used, the tail the returned data (older data) is not expected to be correct.  The reason is that, not all vintages can bee seen, so the clause is no longer true: "the first available datam per specific date of all vintages".
#' @param LookBack how deep in periods to look back for the latest observation in all of the non-oldest vintages.  Meant to use with datasets with a wide range of time between the Measurement interval and the Validity interval.  From the 'Last Updated' date try to peek back in time to the 1st vintage with a published tail 'Date Range' date that is within variable 'LookBack' periods. If the periodicy is "day" and, just after a three(3) day holiday weekend, to reach back from a Tuesday to a Friday, parameter LookBack is increased to a minimum value of 4.  Default is 3. Increase this value if much time exists between the tail date of 'Date Range' and the 'Last Updated' date: meaning zero(0) observations exist in the LookBack period.  The R CRAN package xts function periodicity determines the period of time.  This function is meant to minimize server-side CPU and disk I/O.  . Value can be "Beginning". "Beginning" means lookback to the start.
#' @param VintagesPerQuery number of vintages per HTTPS GET. A.k.a the number of vintages per sheet.   Default is 12.  Common maximum is 12. Value can be "Max". Practical experience has performed with 192.  The maximum may be different during different with not-a-known reason.  This parameter exists to enhance performance by limiting the number of trips to the server.  This parameter is sometimes (but not often) better than the parameter allowParallel. On many occasions  when using this parameter with values greater than 12, the requested data is missing from the returned data set.
#' @param FullOldestVintageData if TRUE, then also return the oldest vintage data and keep(prepend) its data.  Default is FALSE. Useful when 'as much data as possible' is important.
#' @param DataSheet if TRUE, then also return all of the vintages in an xts attribute 'DataSheet'. Default is FALSE.  Useful for debugging.  Useful as a tool of doing more (future) coding or user-end research.
#' @param allowParallel if TRUE, then collect groups of 'sheets of VintagesPerQuery vintages' in parallel.  Default is FALSE.  (Improved) performance will vary: this is more useful on (more data points) weekly data or daily data. Because this is a server side activity, the number of parallel processes does NOT depend on the local machine CPUs.
#' @param MaxParallel if allowParallel is TRUE, then set the maximum number of parallel processes. Default is NULL (no limit).  If this parameter is NULL, then the approximate maximum number of parallel processes is 'unique(ceiling(seq_along(getVintages(SYMBOL)/VintagesPerQuery)))' where the vector from getVintages(SYMBOL) may be reduced by limiting data using EarliestLastUpdDate. Good choices of this parameter may depend on, the amount of the client host hardware CPU and memory.
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
#' # See: getVintages("RECPROUSM156N")
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
#' # same as above, and include the vintageid in the column name
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
#'   c("RECPROUSM156N",  "RECPROUSM156N", "GDP"),           # symbol
#'   c("2020-01-02",     "2019-01-02",    "2019-12-20"),    # vintageid
#'   c(18,                12,              6)               # lookback # months and quarters
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
#' @importFrom xts tclass `tclass<-` tformat `tformat<-` tzone `tzone<-`
### #' `xtsAttributes<-' is not exported by 'namespace:xts'
### #' @importFrom xts xtsAttributes `xtsAttributes<-
#' @importFrom xts as.xts
#' @importFrom quantmod importDefaults getSymbols
getSymbols.ALFRED <- function(Symbols,
                              env,
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

  if(length(returnIndex) && !returnIndex %in% c("ObservationDate", "LastUpdatedDate")) {
    stop("returnIndex must be just one of \"ObservationDate\" or \"LastUpdatedDate\"")
  } else if(!length(returnIndex)) {
    stop("returnIndex can not be NULL")
  }

  if(length(VintageId) && !class(VintageId) %in% c("Date", "character")) {
    stop("VintageId must be NULL or of class \"Date\" or \"character\"")
  } else if(length(VintageId) && class(try( {zoo::as.Date(VintageId)}, silent = F)) == "try-error") {
    stop("VintageId must be NULL or convertible to a Date-like")
  } else if(1L < length(VintageId)) {
    stop("VintageId must be just one element")
  }

  if(length(nameVintagedId) &&!class(nameVintagedId) %in% c("logical")) {
    stop("nameVintagedId must be of class \"logical\"")
  } else if(!length(nameVintagedId)) {
    stop("nameVintagedId can not be NULL")
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

  ALFRED.URL <- "https://alfred.stlouisfed.org/graph/alfredgraph.csv"
  returnSym <- Symbols
  noDataSym <- NULL
  for (i in seq_along(Symbols)) {
    if (verbose)
      cat("downloading ", Symbols[[i]], ".....\n\n")

    # # update (1 of 2 places)
    # returnSym <- returnSym[returnSym %in% Symbols] <- paste0(Symbols[[i]], ".vin")
    # MOVED TO BELOW

    test <- tryCatchLog::tryCatchLog({
    # test <- try({

      # where to get the vintages' 'Last Updated' dates
      if(is.null(VintageId)) {
        AllLastUpdatedDates <- getVintages(Symbols[[i]])
      } else {
        # instead the user chooses the vintages
        AllLastUpdatedDates <- as.character(zoo::as.Date(VintageId))
      }

      # just subtracts off some older 'Last Updated' dates from the results of
      # calling the function getVintages
      if(!is.null(EarliestLastUpdDate)) {
        AllLastUpdatedDates <- zoo::as.Date(AllLastUpdatedDates)[zoo::as.Date(EarliestLastUpdDate) <= zoo::as.Date(AllLastUpdatedDates)]
      }
      # used when nameVintagedId == T
      MostRecentLastUpdatedDate <- utils::tail(AllLastUpdatedDates,1)

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
        URL <- paste(ALFRED.URL, "?id=",           paste0(rep(Symbols[[i]], LengthOfLastUpdatedDates), collapse = ","), sep = "")
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

        h <- curl::new_handle()
        useragent <- paste("curl/", curl::curl_version()$version, " function getSymbols.ALFRED of R CRAN package econModel calling function curl of R CRAN package curl", sep = "")
        # debug in Fiddler 4
        # curl --proxy 127.0.0.1:8888 --insecure -A "custom agent" https://alfred.stlouisfed.org/series/downloaddata?seid=GDP
        # Body dropped from POST request when using proxy with NTLM authentication #146
        # https://github.com/jeroen/curl/issues/146
        # curl::handle_setopt(h, .list = list(proxy = "127.0.0.1", proxyport = 8888, useragent = useragent))
        curl::handle_setopt(h, .list = list(useragent = useragent))
        # go for it
        con <- curl::curl(URL, handle = h)
        fr <- utils::read.csv(con, na.string = ".")
        # docs say that it does not do much
        curl::handle_reset(h)
        # close(con) # DO NOT CLOSE INSIDE foreach::foreach

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
      # of the 'first available datum per specific date' of all vintages,
      #   pull its datum down into a single vector
      #   and format that data as input into package xts function as.xts

      # remove columns that only have NAs seen in EFFR on Holidays
      # if the all-NA columns are not removed then NewCoreData will have problems: "Numeric,0" elements
      # This problem did not happend using na.locf.
      # CLEAN
      NewIndexControl <- apply(FrMatrixTransposedUpsideDown, MARGIN = 2,  function(x) !all(is.na(x)))
      FrMatrixTransposedUpsideDown <- FrMatrixTransposedUpsideDown[, NewIndexControl, drop = F]
      #
      NewCoreData <- matrix(apply(FrMatrixTransposedUpsideDown, MARGIN = 2, function(x) xts::last(stats::na.omit(x))), dimnames = list(colnames(FrMatrixTransposedUpsideDown), NULL))
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

      # redefine (seems must be the same size)
      # coredata(fr) <- NewCoreData
      #
      if(returnIndex == "ObservationDate") {
        # (almost) no change
        NewIndex <- zoo::index(fr)[NewIndexControl]
      }
      if(returnIndex == "LastUpdatedDate") {
        # NewIndexMatrix: not used in default parameter returnCoreData = "ObservationDate"
        NewIndexMatrix <- matrix(apply(FrMatrixTransposedUpsideDown, MARGIN = 2, function(x) { rownames(FrMatrixTransposedUpsideDown)[length(zoo::na.trim(x, sides = "right"))] }  ), dimnames = list(colnames(FrMatrixTransposedUpsideDown), NULL))
        NewIndex       <- zoo::as.Date(sapply(strsplit(as.vector(zoo::coredata(NewIndexMatrix)), "_"), function(x) { x[2] } ), tryFormats = "%Y%m%d")
      }

      # not the same size ( so can not do "coredata(fr) <- NewCoreData")
      frNew <- xts::as.xts(NewCoreData)
      # index(frNew) <- index(fr)
      zoo::index(frNew) <- NewIndex
      xts::xtsAttributes(frNew) <- xts::xtsAttributes(fr)
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
    }, error = function(e) { Sys.setenv(TZ=oldtz); test <- 0L; class(test) <- "try-error"; test  })

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
}
