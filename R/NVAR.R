
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
#' NVAR(matrix())
#' # [1] 0
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
NVAR.matrix <- function(x = NULL) {
tryCatchLog::tryCatchLog({

  if(!identical(x, matrix())) {
    res <- NCOL(x)
  } else {
    res <- 0L
  }
  return(res)

})}


#' @rdname NVAR
#' @examples
#' \dontrun{
#' NVAR(array())
#' # [1] 0
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
NVAR.array <- function(x = NULL) {
tryCatchLog::tryCatchLog({

  if(!identical(x,array())) {
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
    res <- NCOL(coredata(x))
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


