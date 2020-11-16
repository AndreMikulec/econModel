#' econModel: Meaningful Social and Economic Data from ALFRED and elsewhere
#'
#' @section XTS Manipulation and Derivation:
#' \preformatted{
#' explode
#'
#' ifelse.xts
#' is.na.xts
#'
#' timeEvent
#' lagXts
#' absChg
#' relChg
#' diffXts
#' runRanks
#' }
#' @section St. Louis ALFRED (and FRED):
#' \preformatted{
#' fredAttributes
#' estimLastUpdated
#' vinDates
#' getALFRED
#' getSymbols.ALFRED
#'
#' toPeriod
#' }
#' @section Data Combinations:
#' \preformatted{
#' DetectFullRows
#' DetectOnlyNonEmptyVarsInRows
#' DetectEmptyRows
#' RemoveEmptyRows
#' }
#' @section American Association of Individual Investors (AAII)  StockInvestor Pro:
#' \preformatted{
#' getAAIISIProDate
#' copyAAIISIProDBFs
#' }
#' @section Willem Ligtenberg:
#' \preformatted{
#' unlockEnvironment
#' forceAssignInNamespace
#' }
#' @section Miscellaneous:
#' \preformatted{
#' cSort
#' }
#'
#' @docType package
#' @name econModel
"_PACKAGE"
