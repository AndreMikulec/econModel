


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
#' @export
Names <- function(x) {
  if(is.null(x) || !length(x)) return(character(0))
  names(x) -> res
  if(is.null(res)) return(character(0))
  return(res)
}


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
#' @export
`Names<-` <- function(x,value) {

   if(is.null(x)     || !length(x))         x <- character(0)
   if(is.null(value) || !length(value)) value <- character(0)
  `names<-`(x = x, value = value)

}



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
#' @export
#' @importFrom tryCatchLog tryCatchLog
NVAR.default <- function(x) {
tryCatchLog::tryCatchLog({

  if(!length(x)) return(0L)
  NCOL(x)

})}



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
#' @export
#' @importFrom tryCatchLog tryCatchLog
NVAR.xts <- function(x) {
tryCatchLog::tryCatchLog({

  if(length(coredata(x))) {
    res <- NVAR(coredata(x))
  } else {
    res <- 0L
  }
  return(res)

})}





