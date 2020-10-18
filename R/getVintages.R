
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

  if(!length(Symbol)) stop("Must provide the Symbol.")
  if(!length(src))    stop("Invalid source (src).  Default is \"ALFRED\".")

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

