

#' detect rows with non-NA vaues
#'
#' @description
#' \preformatted{
#'
#' complete.cases does not work on tibbles!
#' This function is written in package DataCombine style.
#'
#' }
#'
#' @param x data.frame
#' @param Var columns determining complete cases.
#' Can be column names or positions.
#' Var is the determiner of full rows.
#' Other columns are ignored.
#' @param Ele "All"(default), of elements in Var, the test results.
#' The other option is "Any".
#' @return df with rows of all-NA removed
#' @examples
#' \dontrun{
#' data.frame(A = c(1,NA,NA), B = c(11,12,NA), C = c(101,102,103))
#'    A  B   C
#' 1  1 11 101
#' 2 NA 12 102
#' 3 NA NA 103
#'
#' DetectFullRows(data.frame(A = c(1,NA,NA), B = c(11,12,NA), C = c(101,102,103)),
#'   Var = c("A","B"))
#' [1]  TRUE FALSE FALSE
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
DetectFullRows <- function(x, Var = colnames(x), Ele = "All") {
tryCatchLog::tryCatchLog({

  if(is.null(Var)) stop("\"Var\" can not be NULL")
  if(is.null(Ele)) stop("\"Ele\" can not be NULL")
  if(Ele == "All") Test <- `&`
  if(Ele == "Any") Test <- `|`

  x <- Reduce(Test, lapply(x[, Var, drop = FALSE], function(x2) { !is.na(unlist(x2)) } ))
  x

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' detect rows with columns of non-NA vaues and other columns of non-NA values
#'
#' @description
#' \preformatted{
#'
#' Detect columns(Var) where the values are
#' not-NA (determined by Ele) but
#' the rest of the columns are
#' NA everywhere else (determined by EleO).
#'
#' The function is written in package DataCombine style.
#'
#' }
#'
#' @param x data.frame
#' @param Var columns determining complete cases.
#' Can be column names or positions.
#' @param Ele "All"(default), of elements in Var, the test results.
#' Other option is "Any".
#' @param EleO "All"(default), of elements in 'other than Var', the test results.
#' The other option is "Any".
#' @return df with rows of all-NA removed
#' @examples
#' \dontrun{
#' res <- data.frame(A = c(1,NA,NA,NA,5,6), B = c(11,12,NA,NA,15,NA), C = c(101,102,103,NA,NA,NA))
#' res
#'    A  B   C
#' 1  1 11 101
#' 2 NA 12 102
#' 3 NA NA 103
#' 4 NA NA  NA
#' 5  5 15  NA
#' 6  6 NA  NA
#'
#' DetectOnlyNonEmptyVarsInRows(res, Var = c("A","B"))
#' [1] FALSE FALSE FALSE FALSE  TRUE FALSE
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
DetectOnlyNonEmptyVarsInRows <- function(x, Var = colnames(x), Ele = "All", EleO = "All") {
tryCatchLog::tryCatchLog({

  if(is.null(Var)) stop("\"Var\" can not be NULL")
  if(is.null(Ele)) stop("\"Ele\" can not be NULL")
  if(is.null(Ele)) stop("\"EleO\" can not be NULL")

  # non-NA in Var
  xVar <-  DetectFullRows(x, Var = Var, Ele = Ele)
  # NA in Var
  if(is.numeric(Var))   NotVar <- -Var
  if(is.character(Var)) NotVar <- !colnames(x) %in% Var # TRUE/FALSE
  if(is.logical(Var))   NotVar <- !Var
  xNotVar <- DetectEmptyRows(x, Var = NotVar, Ele = EleO)

  x <- xVar & xNotVar
  x

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}








#' detect rows with NA vaues
#'
#' @description
#' \preformatted{
#'
#' complete.cases does not work on tibbles!
#' This function is written in package DataCombine style.
#'
#' }
#'
#' @param x data.frame
#' @param Var columns determining complete cases.
#' Can be column names or positions.
#' Var is the determiner of empty rows.
#' Other columns are ignored.
#' @param Ele "All"(default), of elements in Var, the test results.
#' The other option is "Any".
#' @examples
#' \dontrun{
#' data.frame(A = c(1,NA,NA), B = c(11,12,NA), C = c(101,102,103))
#'    A  B   C
#' 1  1 11 101
#' 2 NA 12 102
#' 3 NA NA 103
#'
#' DetectEmptyRows(data.frame(A = c(1,NA,NA), B = c(11,12,NA), C = c(101,102,103)),
#'   Var = c("A","B"))
#' [1] FALSE FALSE  TRUE
#' }
#' @return df with rows of all-NA removed
#' @importFrom tryCatchLog tryCatchLog
#' @export
DetectEmptyRows <- function(x, Var = colnames(x), Ele = "All") {
tryCatchLog::tryCatchLog({

  if(is.null(Var)) stop("\"Var\" can not be NULL")
  if(is.null(Ele)) stop("\"Ele\" can not be NULL")
  if(Ele == "All") Test <- `&`
  if(Ele == "Any") Test <- `|`

  x <- Reduce(Test, lapply(x[, Var, drop = FALSE], function(x2) { is.na(unlist(x2)) } ))
  x

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' remove rows with NA vaues
#'
#' @description
#' \preformatted{
#'
#' complete.cases does not work on tibbles!
#' This function is written in package DataCombine style.
#'
#' }
#'
#' @param x data.frame
#' @param Var columns determining complete cases.
#' Can be column names or positions.
#' Var is the determiner of empty rows.
#' Other columns are ignored.
#' @param Ele "All"(default), of elements in Var, the test results
#' Other option is "Any"
#' @examples
#' \dontrun{
#' data.frame(A = c(1,NA,NA), B = c(11,12,NA), C = c(101,102,103))
#'    A  B   C
#' 1  1 11 101
#' 2 NA 12 102
#' 3 NA NA 103
#'
#' RemoveEmptyRows(data.frame(A = c(1,NA,NA), B = c(11,12,NA), C = c(101,102,103)),
#'   Var = c("A","B"))
#'    A  B   C
#' 1  1 11 101
#' 2 NA 12 102
#'
#' RemoveEmptyRows(data.frame(A = c(1,NA,NA), B = c(11,12,NA), C = c(101,102,103)),
#'    Var = c("A","B"), Ele = "Any")
#' A  B   C
#' 1 1 11 101
#' }
#' @return df with rows of all-NA removed
#' @importFrom tryCatchLog tryCatchLog
#' @export
RemoveEmptyRows <- function(x, Var = colnames(x), Ele = "All") {
tryCatchLog::tryCatchLog({

  if(is.null(Var)) stop("\"Var\" can not be NULL")
  if(is.null(Ele)) stop("\"Ele\" can not be NULL")

  # rows to keep
  x <- x[!DetectEmptyRows(x, Var = Var, Ele = Ele),,drop = FALSE]
  log("a")
  x

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}

