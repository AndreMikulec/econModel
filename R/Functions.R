
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
#'             V1na  V2na
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
     Names(xTs) <- paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"na")
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
#' @inheritParams is.na.xts
#' @inherit is.na.xts return details
#' @export
INA <- function(x) {
  is.na.xts(x)
}




#' Lag an xts Object
#'
#' @description
#' \preformatted{
#'
#' This does not complain when: any(abs(k) > NROW(xTs)):
#' zoo_lag can not and will not handle. So k's are eliminated
#' beforehand.
#'
#' }
#' @param x xts object
#' @param k choose -1 to look into the future
#' @param na.pad as lag.xts
#' @param ... dots passed to lag.xts
#' @examples
#' \dontrun{
#'
#' xts(1:4, zoo::as.Date(0:3))
#'            [,1]
#' 1970-01-01    1
#' 1970-01-02    2
#' 1970-01-03    3
#' 1970-01-04    4
#'
#' lagXts(xts(1:4, zoo::as.Date(0:3)), k =  c(-5:-3,0:1,3:5))
#'            V1lead.4 V1lead.3 V1lag.0 V1lag.1 V1lag.3 V1lag.4
#' 1970-01-01       NA        4       1      NA      NA      NA
#' 1970-01-02       NA       NA       2       1      NA      NA
#' 1970-01-03       NA       NA       3       2      NA      NA
#' 1970-01-04       NA       NA       4       3       1      NA
#'
#' xts(matrix(1:4,ncol =2), zoo::as.Date(0:1))
#'            [,1] [,2]
#' 1970-01-01    1    3
#' 1970-01-02    2    4
#'
#' lagXts(xts(matrix(1:4,ncol =2), zoo::as.Date(0:1)), k =  c(-5:-3,0:1,3:5))
#'            V1lag.0 V2lag.0 V1lag.1 V2lag.1
#' 1970-01-01       1       3      NA      NA
#' 1970-01-02       2       4       1       3
#'
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo coredata
#' @importFrom xts xts
#' @importFrom xts lag.xts
#' @export
lagXts <- function(x, k = 1, na.pad = TRUE, ...) {
tryCatchLog::tryCatchLog({

  xTs <- x
  if(all(abs(k) > NROW(xTs))) {
    # do not bother trying
    zoo::coredata(xTs)[] <- NA
    return(xTs)
  }
  # just do the k's within range (that lag.xts  ACCEPTS to handle)
  # CURRENT - I MAY WANT TO CHANGE MY MIND LATER
  IsKineligibleFnd <- abs(k) <= NROW(xTs)
  # CURRENT UNUSED ... if used ... some very very clever interleaving
  kOther <- k[!IsKineligibleFnd]
  k      <- k[ IsKineligibleFnd]
            # lag.xts # lag is not exported from package xts
  xTs <- xts::lag.xts(xTs, k = k, na.pad = na.pad, ...)

  AreColNamesNULL <- if(is.null(colnames(x))) { TRUE } else { FALSE }
  NewColNames <- list()
  for(ki in k) {
    kiName <- abs(ki)
    for(NVARi in seq_len(NVAR(x))){
      if(AreColNamesNULL) {
        ColName <- paste0("V", NVARi)
      } else {
        ColName <- paste0(colnames(x)[NVARi],".")
      }
      if(ki < 0) {
        PostFix <- "lead"
      } else {
        PostFix <- "lag"
      }
      NewColNamesI <- paste0(ColName, PostFix, ".", kiName)
      NewColNames <- c(list(), NewColNames, list(NewColNamesI))
    }

  }
  # better names
  colnames(xTs) <- DescTools::DoCall(c, NewColNames)
  xTs

})}


#' Lag an xts Object
#'
#' @description
#' \preformatted{
#'
#' This does not complain when: any(abs(k) > NROW(xTs)):
#' zoo_lag can not and will not handle. So k's are eliminated
#' beforehand.
#'
#' }
#' @examples
#' \dontrun{
#'
#' # LG(lagXts lagging 1) example
#'
#' LG(IBM.Open.TTR.SMA.n.2, k = 1)
#'            IBM.Open.TTR.SMA.n.2.lag.1
#' 1970-01-02                         NA
#' 1970-01-05                         NA
#' 1970-01-06                   18.26250
#' 1970-01-07                   18.35625
#' 1970-01-08                   18.41875
#' 1970-01-09                   18.43125
#' 1970-01-12                   18.45625
#' }
#' @inheritParams lagXts
#' @param k choose 0 or 1 or greater to peer into the current and the past
#' @inherit lagXts return details
#' @export
LG <- function(x, k = 1, na.pad = TRUE, ...) {
   if( any(unlist(lapply (k, function(k1) { k1 < 0  } )))) {
     stop("k < 0: fix this; Use function LD(lead) instead")
   }
   lagXts(x, k = k, na.pad = na.pad, ...)
}


#' Lead an xts Object
#'
#' @description
#' \preformatted{
#'
#' This does not complain when: any(abs(k) > NROW(xTs)):
#' zoo_lag can not and will not handle. So k's are eliminated
#' beforehand.
#'
#' }
#' @examples
#' \dontrun{
#'
#' # LD(lagXts leading 1) example
#'
#' LD(IBM.Open.TTR.SMA.n.2, k = 1)
#'            IBM.Open.TTR.SMA.n.2.lead.1
#' 1970-01-02                    18.26250
#' 1970-01-05                    18.35625
#' 1970-01-06                    18.41875
#' 1970-01-07                    18.43125
#' 1970-01-08                    18.45625
#' 1970-01-09                    18.46250
#' 1970-01-12                          NA
#'
#' }
#' @inheritParams lagXts
#' @param k choose 1 or greater  to look into the future
#' @inherit lagXts return details
#' @export
LD <- function(x, k = 1, na.pad = TRUE, ...) {
   if( any(unlist(lapply (k, function(k1) { k1 <= 0  } )))) {
     stop("k <= 0: fix this; Use function LG(lag) instead")
   }
   lagXts(x, k = -1 * k, na.pad = na.pad, ...)
}




#' Absolute Change
#'
#' @description
#' \preformatted{
#' }
#' @param x xts object
#' @param base choose -1 (or less) to look into the future
#' @param lag observations backwards
#' @param ... dots passed to lagXts
#' @examples
#' \dontrun{
#'
#' # AC example
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#' #'
#' AC(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)))
#'            V1ac.1 V2ac.1
#' 1970-01-01     NA     NA
#' 1970-01-02     -3      8
#' 1970-01-03     -2     16
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_replace
#' @export
AC <- function(x, base = 0, lag = 1, ...) {
tryCatchLog::tryCatchLog({

  xTs <- x
  xTs1 <- lagXts(xTs, k = base + rep(0,length(lag)), ...)
  xTs2 <- lagXts(xTs, k = base + lag, ...)
  xTs  <- xTs1 - xTs2
  # strait override
  if(NVAR(xTs)) {
     Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"ac"),".", lag)
  }
  xTs
})}








#' Lag and/or Difference and/or Use a function(Fun) Upon its xts Object
#'
#' @description
#' \preformatted{
#' Based on R CRAN package xts function diff.xts
#' }
#' @param x as diff.xts
#' @param lag as diff.xts
#' @param differences as diff.xts
#' @param arithmetic as diff.xts
#' @param log as diff.xts
#' @param na.pad as diff.xts
#' @param Fun difference-ing function.
#' Meant to change the xTs in some way.
#' (Default diff (expected: xts::diff.xts)).
#' Should accept or (accept and ignore) the parameters: lag;
#' for S3 compatibility, differences; for xts compatiblity,
#' arithmetic, log, and/or na.pad.
#' @examples
#' \dontrun{
#'
#' # diffXts example
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#'
#'
#' diffXts(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)))
#'         V1diff.1 V2diff.1
#' 1970-01-01       NA       NA
#' 1970-01-02       -3        8
#' 1970-01-03       -2       16
#'
#' }
#' @param ... dots passed
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @export
diffXts <- function(x, lag=1, differences=1, arithmetic=TRUE, log=FALSE, na.pad=TRUE, Fun = diff, ...) {
tryCatchLog::tryCatchLog({

  Fun = match.fun(Fun)
  Dots <- list(...)

  if(!is.null(lag)) {
    if(!is.integer(lag) && any(is.na(as.integer(lag))))
      stop("'lag' must be integer")
  }

  if(!is.null(differences)) {
    differences <- as.integer(differences[1L])
    if(is.na(differences))
      stop("'differences' must be integer")
  }
  init.differences <- differences

  if(is.logical(x))
    x <- .xts(matrix(as.integer(x),ncol=NCOL(x)), .index(x), tclass(x))

  # if the use is to wants to do some differencing
  if(!is.null(differences) && !is.na(differences) && is.numeric(differences)) {

    # basically just keep iterative-ly keep running the same function
    # differences is a 'dumb counter'
    if(differences > 1) {
      xTs <- DescTools::DoCall(Fun, c(list(), list(x),     lag=lag,   arithmetic=arithmetic, log = log, na.pad = na.pad, Fun = Fun, Dots))
      diffXts(xTs, lag=lag, differences=differences - 1,              arithmetic=arithmetic, log = log, na.pad = na.pad, Fun = Fun,   ...)
    } else {
      xTs <- DescTools::DoCall(Fun,  c(list(), list(x),    lag=lag,   arithmetic=arithmetic, log = log, na.pad = na.pad, Fun = Fun, Dots))
      if(!length(Names(xTs)) && NVAR(xTs)) {
         Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"diff"),".", init.differences)
      }
      return(xTs)
    }

  }

})}



