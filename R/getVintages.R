
#' Download vintage dates of Federal Reserve Economic Data - ALFRED(R)
#'
#' Downloads vintage dates of the published tail dates of 'Date Range' of the specified Symbol from 'research.stlouisfed.org'.
#'
#' Used internally by the function getSymbols.ALFRED
#'
#' Note, this function does not use the official API.  The official API usage can be performed using the R CRAN package alfred function get_alfred_series.  In some cases, this function getVintages may be faster.
#'
#' @param Symbol specifying the name of the symbol to be loaded
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
getVintages <- function(Symbol, src = "ALFRED") {
tryCatchLog::tryCatchLog({

  if(!length(Symbol)) stop("Must provide the Symbol.")
  if(!length(src))    stop("Invalid source (src).  Default is \"ALFRED\".")

  if(src == "ALFRED") {
    URL <- paste0("https://alfred.stlouisfed.org/series/downloaddata?seid=", Symbol)
    # scrape
    Page <- paste0(scan(URL, what = character()), collapse = " ")
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

