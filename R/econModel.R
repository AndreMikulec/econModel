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
#' diffXts
#' runRanks
#'
#' These avoid the R CRAN package TTR problem
#' Error in runSum(x, n) : Series contains non-leading NAs
#' absChg
#' relChg
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
#' @section Modeling:
#' \preformatted{
#' specifyModel
#' buildModel
#' predictModel
#' }
#' @docType package
#' @name econModel
"_PACKAGE"
