



#' better Names
#'
#' @description
#' \preformatted{
#'
#' If the argument to names is NULL or has zero(0) length
#' then instead for returning NULL, return character(0).
#'
#' }
#'
#' @param x names
#' @export
Names <- function(x) {
  if(is.null(x) || !length(x)) return(character(0))
  names(x) -> res
  if(is.null(res)) return(character(0))
  return(res)
}



#' garantee that Names assigns a zero length character vector
#'
#' @description
#' \preformatted{
#'
#' if x or value is null or has length zero(0) then
#' then that item is character(0)
#'
#' }
#'
#' @param x names
#' @param value result
#' @export
`Names<-` <- function(x,value) {

   if(is.null(x)     || !length(x))         x <- character(0)
   if(is.null(value) || !length(value)) value <- character(0)
  `names<-`(x = x, value = value)

}



#' number of variables
#'
#' @description
#' \preformatted{
#'
#' NCOL wrongly returns value one(1)
#' on non-data.frame 2nd dimension objects
#' with a 2nd dimension size of zero(0).
#'
#' }
#' @param x object
#' @return integer number of variables
#' @rdname NVAR
#' @export
NVAR <- function(x = NULL) {
  # tryCatchLog is not allowed here
  UseMethod("NVAR")
}



#' @rdname NVAR
#' @examples
#' \dontrun{
#' NVAR(character())
#' # [1] 0
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
NVAR.character <- function(x = NULL) {
tryCatchLog::tryCatchLog({

  if(!identical(x, character())) {
    res <- NCOL(x)
  } else {
    res <- 0L
  }
  return(res)

})}



#' @rdname NVAR
#' @examples
#' \dontrun{
#' NVAR(integer())
#' # [1] 0
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
NVAR.integer <- function(x = NULL) {
tryCatchLog::tryCatchLog({

  if(!identical(x, integer())) {
    res <- NCOL(x)
  } else {
    res <- 0L
  }
  return(res)

})}


#' @rdname NVAR
#' @examples
#' \dontrun{
#' NVAR(numeric())
#' # [1] 0
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
NVAR.numeric <- function(x = NULL) {
tryCatchLog::tryCatchLog({

  if(!identical(x, numeric())) {
    res <- NCOL(x)
  } else {
    res <- 0L
  }
  return(res)

})}


#' @rdname NVAR
#' @examples
#' \dontrun{
#' NVAR(integer())
#' # [1] 0
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
NVAR.integer <- function(x = NULL) {
tryCatchLog::tryCatchLog({

  if(!identical(x, integer())) {
    res <- NCOL(x)
  } else {
    res <- 0L
  }
  return(res)

})}


#' @rdname NVAR
#' @examples
#' \dontrun{
#' NVAR(complex())
#' # [1] 0
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
NVAR.complex <- function(x = NULL) {
tryCatchLog::tryCatchLog({

  if(!identical(x, complex())) {
    res <- NCOL(x)
  } else {
    res <- 0L
  }
  return(res)

})}


#' @rdname NVAR
#' @examples
#' \dontrun{
#' NVAR(raw())
#' # [1] 0
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
NVAR.raw <- function(x = NULL) {
tryCatchLog::tryCatchLog({

  if(!identical(x, raw())) {
    res <- NCOL(x)
  } else {
    res <- 0L
  }
  return(res)

})}







#' @rdname NVAR
#' @examples
#' \dontrun{
#' # > library(xts)
#' # > NVAR(xts(, as.Date("1970-01-12")))
#' # [1] 0
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
NVAR.xts <- function(x = NULL) {
tryCatchLog::tryCatchLog({

  if(length(coredata(x))) {
    res <- NVAR(coredata(x))
  } else {
    res <- 0L
  }
  return(res)

})}


#' @rdname NVAR
#' @export
#' @importFrom tryCatchLog tryCatchLog
NVAR.default <- function(x = NULL) {
tryCatchLog::tryCatchLog({

  if(is.null(x)) return(0L)
  NCOL(x)

})}


