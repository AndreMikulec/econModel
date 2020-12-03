

#' Print Proper (and short)
#'
#' @description
#' \preformatted{
#' Print Proper (and short)
#' }
#' @param x object
#' @param ... dots passed
#' @return x object or "print proper" string
#' @examples
#' \dontrun{
#'
#' # prntPrpr(Print Proper (and short)) example
#'
#' prntPrpr(T)
#' [1] "T"
#'
#' prntPrpr(c(T,F,NA))
#' [1] "T" "F" "N"
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
prntPrpr <- function(x, ...) {
tryCatchLog::tryCatchLog({

  if(1 < length(x)) {
    return(unlist(lapply(x, prntPrpr, ...)))
  }
  Done <- FALSE

  if(!Done && is.na(x)) {
    Prnt <- "N"; Done <- T
  }
  if(!Done && is.logical(x)) {
    if(x) {
      Prnt <- "T"; Done <- T
    } else {
      Prnt <- "F" ; Done <- T
    }
  }
  if(!Done)
  {
    Prnt <- x; Done <- T
  }
  Prnt
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


#' Better Names
#'
#' @description
#' \preformatted{
#' Names:
#' If the argument to names is NULL or has zero(0) length
#' then instead for returning NULL, return character(0).
#' }
#' @param x names
#' @rdname Names
#' @importFrom tryCatchLog tryCatchLog
#' @export
Names <- function(x) {
tryCatchLog::tryCatchLog({
  if(is.null(x) || !length(x)) return(character(0))
  names(x) -> res
  if(is.null(res)) return(character(0))
  return(res)
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


#' Better Names
#'#'
#' @description
#' \preformatted{
#' `Names<-`
#' Guarantee that Names assigns a zero length character vector.
#' If x or value is null or has length zero(0) then
#' then that item is character(0).
#' }
#' @param x names
#' @param value result
#' @rdname Names
#' @importFrom tryCatchLog tryCatchLog
#' @export
`Names<-` <- function(x,value) {
tryCatchLog::tryCatchLog({
   if(is.null(x)     || !length(x))         x <- character(0)
   if(is.null(value) || !length(value)) value <- character(0)
  `names<-`(x = x, value = value)
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Number of Variables
#'
#' @description
#' \preformatted{
#' NCOL wrongly returns value one(1)
#' on non-data.frame 2nd dimension objects
#' with a 2nd dimension size of zero(0).
#'
#' This function NVAR correctly determines the number
#' of variables (NVAR) in an object.
#'
#' R S3 class implementations exist for classes: xts
#'
#' Contributions are welcome.
#' }
#' @param x object
#' @return integer number of variables
#' @rdname NVAR
#' @export
NVAR <- function(x) {
  # tryCatchLog is not allowed here
  UseMethod("NVAR")
}


#' Number of Variables
#'
#' @description
#' \preformatted{
#' }
#' @rdname NVAR
#' @examples
#' \dontrun{
#'
#' # examples
#'
#' NVAR(NULL)
#' [1] 0
#'
#' NVAR(numeric())
#' [1] 0
#'
#' NVAR(integer())
#' [1] 0
#'
#' NVAR(character())
#' [1] 0
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
NVAR.default <- function(x) {
tryCatchLog::tryCatchLog({

  if(!length(x)) return(0L)
  NCOL(x)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' @rdname NVAR
#' @examples
#' \dontrun{
#'
#' # xts example
#'
#' library(xts)
#' NVAR(xts(, as.Date("1970-01-01")))
#' [1] 0
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
NVAR.xts <- function(x) {
tryCatchLog::tryCatchLog({

  if(length(coredata(x))) {
    res <- NVAR(coredata(x))
  } else {
    res <- 0L
  }
  return(res)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' If Then Else
#'
#' @description
#' \preformatted{
#' If Then Else
#' }
#' @param test xts object
#' @param yes if T, select this value
#' @param no  if F, select this value
#' @param ... dots passed
#' @return xts object
#' @examples
#' \dontrun{
#'
#' # ifelse.xts(If Then Else) examples
#'
#' xts(matrix(c(T,F),ncol=1), zoo::as.Date(0:1))
#'            [,1]
#' 1970-01-01  TRUE
#' 1970-01-02 FALSE
#'
#' ifelse.xts(xts(matrix(c(T,F),ncol=1), zoo::as.Date(0:1)), 11, 10)
#'            V1ie
#' 1970-01-01   11
#' 1970-01-02   10
#'
#' xts(matrix(c(T,F,F,T),ncol=2), zoo::as.Date(0:1))
#'             [,1]  [,2]
#' 1970-01-01  TRUE FALSE
#' 1970-01-02 FALSE  TRUE
#'
#' ifelse.xts(xts(matrix(c(T,F,F,T),ncol=2), zoo::as.Date(0:1)), 11, 10)
#'            V1ie V2ie
#' 1970-01-01   11   10
#' 1970-01-02   10   11
#'
#' ifelse.xts(xts(matrix(c(T,F,F,T),ncol=2), zoo::as.Date(0:1)), c(11,21), c(10,20))
#'            V1ie V2ie
#' 1970-01-01   11   10
#' 1970-01-02   20   21
#'
#' xts(matrix(c(11,10),ncol=1), zoo::as.Date(0:1))
#'            [,1]
#' 1970-01-01   11
#' 1970-01-02   10
#'
#' # note: some functions, I must test on the coredata. E.g. is.na
#' ifelse.xts(xts(matrix(c(11,10),ncol=1), zoo::as.Date(0:1)) > 10 , 21, 20)
#'            V1ie
#' 1970-01-01   21
#' 1970-01-02   20
#' }
#' @importFrom tryCatchLog  tryCatchLog
#' @importFrom zoo coredata
#' @importFrom data.table fifelse
#' @export
ifelse.xts <- function(test, yes, no, ...) {
tryCatchLog::tryCatchLog({

  # ifelse.zoo is not a method (because ifelse is not a generic)
  # ? zoo
  xTs <- test
  zoo::coredata(xTs) <- apply(zoo::coredata(xTs), MARGIN = 2, FUN = function(x) {
    data.table::fifelse(x, yes, no)
  })
  # strait override
  if(NVAR(xTs)) {
     Names(xTs) <- paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"ie")
  }
  xTs

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' If Then Else
#'
#' @description
#' \preformatted{
#' If Then Else
#' }
#' @param x xts object
#' @param y if T, select this value
#' @param n  if F, select this value
#' @param ... dots passed
#' @return xts object

#' @examples
#' \dontrun{
#'
#' # IE(If Then Else) examples
#'
#' xts(matrix(c(T,F),ncol=1), zoo::as.Date(0:1))
#'            [,1]
#' 1970-01-01  TRUE
#' 1970-01-02 FALSE
#'
#' IE(xts(matrix(c(T,F),ncol=1), zoo::as.Date(0:1)), 11, 10)
#'            V1ie
#' 1970-01-01   11
#' 1970-01-02   10
#'
#' matrix(c(T,F,F,T),ncol=2), zoo::as.Date(0:1)
#'             [,1]  [,2]
#' 1970-01-01  TRUE FALSE
#' 1970-01-02 FALSE  TRUE
#'
#' IE(xts(matrix(c(T,F,F,T),ncol=2), zoo::as.Date(0:1)), 11, 10)
#'            V1ie V2ie
#' 1970-01-01   11   10
#' 1970-01-02   10   11
#'
#' IE(xts(matrix(c(T,F,F,T),ncol=2), zoo::as.Date(0:1)), c(11,21), c(10,20))
#'            V1ie V2ie
#' 1970-01-01   11   10
#' 1970-01-02   20   21
#'
#' xts(matrix(c(11,10),ncol=1), zoo::as.Date(0:1))
#'            [,1]
#' 1970-01-01   11
#' 1970-01-02   10
#'
#' # note: some functions, I must test on the coredata. E.g. is.na
#' IE(xts(matrix(c(11,10),ncol=1), zoo::as.Date(0:1)) > 10 , 21, 20)
#'            V1ie
#' 1970-01-01   21
#' 1970-01-02   20
#' }
#' @importFrom tryCatchLog  tryCatchLog
#' @importFrom zoo coredata
#' @export
IE <- function(x, y, n, ...) {
tryCatchLog::tryCatchLog({

  xTs <- ifelse.xts(x, yes = y, no = n)
  # strait override
  if(NVAR(xTs)) {
     Names(xTs) <- paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"ie")
  }
  xTs

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


