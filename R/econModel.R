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
#' @section Acquire (and Manipulate) St. Louis ALFRED (and FRED) Data:
#' \preformatted{
#' fredAttributes
#' estimLastUpdated
#' vinDates
#' getFRED
#' getALFRED
#' getSymbols.ALFRED
#' toPeriod
#' }
#' @section Detect (and Remove) Data Combinations:
#' \preformatted{
#' DetectFullRows
#' DetectOnlyNonEmptyVarsInRows
#' DetectEmptyRows
#' RemoveEmptyRows
#' }
#' @section American Association of Individual Investors (AAII) StockInvestor Pro:
#' \preformatted{
#' getAAIISIProDate
#' copyAAIISIProDBFs
#' }
#' @section Unlock a Pathed/Loaded R Package:
#' \preformatted{
#' unlockEnvironment
#' forceAssignInNamespace
#' lsNamespaceInfo
#' AllInfoNS
#' }
#' @section Cast a Data.frame Into an xts Object (and Back Again):
#' \preformatted{
#' castDf2Xts
#' meltXts2Df
#' }
#' @section Miscellaneous - Custom Sort:
#' \preformatted{
#' cSort
#' }
#'
#' @docType package
#' @name econModel
"_PACKAGE"
