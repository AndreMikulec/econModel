
#' Perform is.na About Each Column in an xts Object
#'
#' @description
#' \preformatted{
#' Without this function, is.na(xts) strips the index.
#' Probably this function should be an xts enhancement.
#' }
#' @param x xts object
#' @return xts object of T/F values
#' @examples
#' \dontrun{
#'
#' is.na.xts examples
#'
#' library(xts)
#'
#' xts(c(NA_real_, 0, NA_real_), zoo::as.Date(0:2))
#'
#'            [,1]
#' 1970-01-01   NA
#' 1970-01-02    0
#' 1970-01-03   NA
#'
#' is.na(xts(c(NA_real_, 0, NA_real_), zoo::as.Date(0:2)))
#'
#'             [,1]
#' 1970-01-01  TRUE
#' 1970-01-02 FALSE
#' 1970-01-03  TRUE
#'
#' NOTE
#' is.na(xts::xts(matrix(c(NA_real_, 0, NA_real_,0), ncol = 2), zoo::as.Date(0:1)))
#'             [,1]  [,2]
#' 1970-01-01  TRUE  TRUE
#' 1970-01-02 FALSE FALSE
#'
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo coredata index
#' @importFrom xts xts
is.na.xts <- function(x) {
tryCatchLog::tryCatchLog({

  # without this function, is.na(xts) strips the index
  # probably this function should be an xts enhancement
  #
  xTs <- xts::xts(is.na(zoo::coredata(x)), index(x))
  if(!length(Names(xTs)) && NVAR(xTs)) {
     Names(xTs) <- paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs)))
  }
  xTs

})}



#' Perform is.na About Each Column in an xts Object
#'
#' @description
#' \preformatted{
#' Without the function, INA, is.na(xts) strips the index.
#' Probably this function should be an xts enhancement.
#' }
#' @examples
#' \dontrun{
#'
#' # INA examples
#'
#' library(xts)
#'
#' xts(c(NA_real_, 0, NA_real_), zoo::as.Date(0:2))
#'
#'            [,1]
#' 1970-01-01   NA
#' 1970-01-02    0
#' 1970-01-03   NA
#'
#' INA(xts(c(NA_real_, 0, NA_real_), zoo::as.Date(0:2)))
#'
#'               V1
#' 1970-01-01  TRUE
#' 1970-01-02 FALSE
#' 1970-01-03  TRUE
#'
#' NOTE
#' INA(xts::xts(matrix(c(NA_real_, 0, NA_real_,0), ncol = 2), zoo::as.Date(0:1)))
#'              V1     V2
#' 1970-01-01  TRUE  TRUE
#' 1970-01-02 FALSE FALSE
#'
#' explode(IBM.Open.TTR.SMA.n.2, Fun = "INA")
#'            IBM.Open.TTR.SMA.n.2.INA
#' 1970-01-02                     TRUE
#' 1970-01-05                    FALSE
#' 1970-01-06                    FALSE
#' 1970-01-07                    FALSE
#' 1970-01-08                    FALSE
#' 1970-01-09                    FALSE
#' 1970-01-12                    FALSE
#'
#' }
#' @rdname TTRLikeFunctions
#' @inheritParams is.na.xts
#' @inherit is.na.xts return details
#' @export
INA <- function(x) {
  is.na.xts(x)
}



