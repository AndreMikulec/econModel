
#' Perform is.na About Each Column in an xts Object
#'
#' @description
#' \preformatted{
#' Without this function, is.na(xts) strips the index.
#' Probably this function should be an xts enhancement.
#' }
#' @param x xts object
#' @param ... dots passes
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
is.na.xts <- function(x, ...) {
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
#' # INA(is na?) examples
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
#'            V1ina
#' 1970-01-01  TRUE
#' 1970-01-02 FALSE
#' 1970-01-03  TRUE
#'
#' INA(xts::xts(matrix(c(NA_real_, 0, NA_real_,0), ncol = 2), zoo::as.Date(0:1)))
#'            V1ina V2ina
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
#' }
#' @inheritParams is.na.xts
#' @inherit is.na.xts return details
#' @export
INA <- function(x, ...) {
tryCatchLog::tryCatchLog({

  xTs <- is.na.xts(x)
  # strait override
  if(NVAR(xTs)) {
     Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"ina"))
  }
  xTs

})}




#' Lag an xts Object
#'
#' @description
#' \preformatted{
#' This does not complain when: any(abs(k) > NROW(xTs)):
#' zoo_lag can not and will not handle. So k's are eliminated
#' beforehand.
#' }
#' @param x xts object
#' @param k choose -1 to look into the future
#' @param na.pad as lag.xts
#' @param ... dots passed to lag.xts
#' @return xts object
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
#' This does not complain when: any(abs(k) > NROW(xTs)):
#' zoo_lag can not and will not handle. So k's are eliminated
#' beforehand.
#' }
#' @param x xts object
#' @param k choose 0 or 1 or greater to peer into the current and the past
#' @param ... dots passed
#' @return xts object
#' @examples
#' \dontrun{
#'
#' # LG(lag 1) example
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#'
#' LG(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)))
#'            V1lg.1 V2lg.1
#' 1970-01-01     NA     NA
#' 1970-01-02      1      8
#' 1970-01-03     -2     16
#' }

#' @inherit lagXts return details
#' @export
LG <- function(x, k = 1, ...) {
tryCatchLog::tryCatchLog({

   if( any(unlist(lapply (k, function(k1) { k1 < 0 } )))) {
     stop("k < 0: fix this; Use function LD(lead) instead")
   }
   xTs <- lagXts(x, k = k, na.pad = TRUE, ...)
  # strait override
  if(NVAR(xTs)) {
     Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"lg"),".", k)
  }
  xTs
})}


#' Lead an xts Object
#'
#' @description
#' \preformatted{
#' This does not complain when: any(abs(k) > NROW(xTs)):
#' zoo_lag can not and will not handle. So k's are eliminated
#' beforehand.
#' }
#' @param x xts object
#' @param k choose 1 or greater  to look into the future
#' @param ... dots passed
#' @return xts object
#' @examples
#' \dontrun{
#'
#' # LD(lead 1) example
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#'
#' LD(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)))
#'            V1ld.1 V2ld.1
#' 1970-01-01     -2     16
#' 1970-01-02     -4     32
#' 1970-01-03     NA     NA
#' }
#' @inherit lagXts return details
#' @export
LD <- function(x, k = 1, ...) {
tryCatchLog::tryCatchLog({

   if( any(unlist(lapply (k, function(k1) { k1 <= 0  } )))) {
     stop("k <= 0: fix this; Use function LG(lag) instead")
   }
   xTs <- lagXts(x, k = -1 * k, na.pad = TRUE, ...)
  # strait override
  if(NVAR(xTs)) {
     Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"ld"),".", k)
  }
  xTs

})}




#' Absolute Change
#'
#' @description
#' \preformatted{
#' Absolute Change
#' }
#' @param x xts object
#' @param base choose -1 (or less) to look into the future
#' @param lag observations backwards
#' @param ... dots passed to lagXts
#' @return xts object
#' @examples
#' \dontrun{
#'
#' # AC(absolute change) example
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#' #'
#' absChg(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)))
#'            V1ac.0.1 V2ac.0.1
#' 1970-01-01       NA       NA
#' 1970-01-02       -3        8
#' 1970-01-03       -2       16
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
absChg <- function(x, base = 0, lag = 1, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)
  if(!is.null(Dots$b))  { base <- Dots$b }
  if(!is.null(Dots$l))  { lag  <- Dots$l }

  xTs <- x
  xTs1 <- lagXts(xTs, k = base + rep(0,length(lag)), ...)
  xTs2 <- lagXts(xTs, k = base + lag, ...)
  xTs  <- xTs1 - xTs2
  # strait override
  if(NVAR(xTs)) {
     Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"ac"),".", base, ".", lag)
  }
  xTs
})}



#' Absolute Change
#'
#' @description
#' \preformatted{
#' Absolute Change
#' }
#' @param x xts object
#' @param b choose -1 (or less) to look into the future
#' @param l observations backwards
#' @param ... dots passed to lagXts
#' @return xts object
#' @examples
#' \dontrun{
#'
#' # AC(absolute change) example
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#' #'
#' AC(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)))
#'            V1ac.0.1 V2ac.0.1
#' 1970-01-01       NA       NA
#' 1970-01-02       -3        8
#' 1970-01-03       -2       16
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
AC <- function(x, b = 0, l = 1, ...) {
tryCatchLog::tryCatchLog({

  xTs <- absChg(x, base = b, lag = l, ...)
  # strait override
  if(NVAR(xTs)) {
     Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"ac"),".", b, ".", l)
  }
  xTs
})}




#' Absolute Proportional Change
#'
#' @description
#' \preformatted{
#' This is most useful for calculating velocity
#' and acceleration, (and jerk).
#' To get acceleration and jerk use this with
#' diffXts with differences 2 (and 3) respectively.
#'
#' Ninety-nine (99%) percent of the people in the world
#' SHOULD HAVE BEEN using this one.
#' }
#' @param x xts object
#' @param b choose -1 (or less) to look into the future
#' @param l observations backwards
#' @param ... dots passed
#' @return xts object
#' @examples
#' \dontrun{
#'
#' # APC(Absolute Proportional Change) example
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#'
#' APC(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)))
#'            V1apc.0.1 V2apc.0.1
#' 1970-01-01        NA        NA
#' 1970-01-02        -3         1
#' 1970-01-03        -1         1
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
APC <- function(x, b = 0, l = 1, ...) {
tryCatchLog::tryCatchLog({

  xTs <- x
  xTs2 <- lagXts(xTs, k = b + l, ...)
  xTs  <- AC(xTs, b = b + rep(0,length(l)), l = l, ...)/ abs(xTs2)
  # strait override
  if(NVAR(xTs)) {
     Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"apc"),".", b, ".", l)
  }
  xTs
})}



#' Absolute Percent Change
#'
#' @description
#' \preformatted{
#' Absolute Proportional Change(APC) multiplied by 100.00
#' Absolute Proportional Change(APC) in percent (APCP)
#' }
#' @inheritParams APC
#' @inherit APC return details
#' @examples
#' \dontrun{
#'
#' # APCP(Absolute Proportional Change in Percent) example
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#'
#' APCP(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)))
#'            V1apcp.0.1 V2apcp.0.1
#' 1970-01-01         NA         NA
#' 1970-01-02       -300        100
#' 1970-01-03       -100        100
#' }
#' @importFrom tryCatchLog tryCatchLog
APCP <- function(x, b = 0, l = 1, ...) {
tryCatchLog::tryCatchLog({

  xTs <- APC(x, b = b, l = l, ...) * 100.000
  # strait override (I know that xTs has no names)
  if(NVAR(xTs)) {
     Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"apcp"),".", b, ".", l)
  }
  xTs
})}



#' Relative Proportional Change
#'
#' @description
#' \preformatted{
#' Ninety-nine (99%) percent of the people in the world
#' are using THIS WRONG ONE.
#' }
#' @param x xts object
#' @param b choose -1 (or less) to look into the future
#' @param l observations backwards
#' @param ... dots passed
#' @return xts object
#' @examples
#' \dontrun{
#'
#' RPC(Relative Proportional Change) example
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#'
#' RPC(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)))
#'            V1rpc.0.1 V2rpc.0.1
#' 1970-01-01        NA        NA
#' 1970-01-02        -3         1
#' 1970-01-03         1         1
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
RPC <- function(x, b = 0, l = 1, ...) {
tryCatchLog::tryCatchLog({

  xTs <- x
  xTs2 <- lagXts(xTs, k = b + l, ...)
  xTs <- AC(xTs, b = b + rep(0,length(l)), l = l, ...)/xTs2
  # strait override
  if(NVAR(xTs)) {
     Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"rpc"),".", b, ".", l)
  }
  xTs
})}




#' Relative Proportional Change in Percent
#'
#' @description
#' \preformatted{
#' Ninety-nine (99%) percent of the people in the world
#' are using THIS WRONG ONE.
#' Relative Proportional Change(RPC) multiplied by 100.00
#' Relative Proportional Change(RPC) in percent (RPCP)
#' }
#' @inheritParams RPC
#' @inherit RPC return details
#' @examples
#' \dontrun{
#'
#' RPCP(Relative Proportional Change in Percent) example
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#'
#' RPCP(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)))
#'            V1rpcp.0.1 V2rpcp.0.1
#' 1970-01-01         NA         NA
#' 1970-01-02       -300        100
#' 1970-01-03        100        100
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
RPCP <- function(x, b = 0, l = 1, ...) {
tryCatchLog::tryCatchLog({

  xTs <- RPC(x, b = b, l = l, ...) * 100
  # strait override
  if(NVAR(xTs)) {
     Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"rpcp"),".", b, ".", l)
  }
  xTs
})}




#' Relative Change
#'
#' @description
#' \preformatted{
#' Relative Change
#' }
#' @param x xts object
#' @param base choose -1 (or less) to look into the future
#' @param lag observations backwards
#' @param log logrithmic
#' @param ... dots passed
#' @return xts object
#' @examples
#' \dontrun{
#'
#' # relChg (relative change) example
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#'
#' relChg(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)))
#'            V1rc.0.1 V2rc.0.1
#' 1970-01-01       NA       NA
#' 1970-01-02       -2        2
#' 1970-01-03       -1        2
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo coredata
#' @export
relChg <- function(x = NULL, base = 0, lag = 1, log = FALSE, ...) {
tryCatchLog::tryCatchLog({

  xTs <- x
  xTs1 <- lagXts(xTs, k = base + rep(0,length(lag)), ...)
  xTs2 <- lagXts(xTs, k = base + lag               , ...)

  # ? `[`
  #
  # x[i]
  # x[i] <- value
  #
  # A third form of indexing is via a
  # numeric matrix with the
  #   one column for each dimension: each row of the index matrix
  #     then selects a single element of the array,
  #       and the result is a vector.
  # Negative indices are not allowed in the index matrix.
  # NA and zero values are allowed:
  #   rows of an index matrix containing a zero are ignored,
  #   whereas rows containing an NA produce an NA in the result.
  #
  # Indexing via a character matrix with one column per dimensions is also supported
  # if the array has dimension names.
  #   As with numeric matrix indexing, each row of the index matrix selects a single element of the array.
  #   Indices are matched against the appropriate dimension names. N
  #   A is allowed and will produce an NA in the result.
  #   Unmatched indices as well as the empty string ("") are not allowed and will result in an error.

  NegNegTest <- (lagXts(xTs, k = base + rep(0,length(lag)), ...) < 0) & (lagXts(xTs, k = base + lag, ...) < 0)

  # before any 'any/all' test
  if(anyNA(NegNegTest))
    NegNegTest[which(is.na(NegNegTest), arr.ind = TRUE)] <- FALSE

  if(any(zoo::coredata(NegNegTest) == TRUE)) {

    NewCoreXts <- zoo::coredata(xTs)

    arrayIndiciesNegNeg  <- which( zoo::coredata(NegNegTest), arr.ind = TRUE)
    arrayIndiciesRegular <- which(!zoo::coredata(NegNegTest), arr.ind = TRUE)

    # regular common case
    NewCoreXts[arrayIndiciesRegular] <-
                  zoo::coredata(xTs1)[arrayIndiciesRegular]/
                  zoo::coredata(xTs2)[arrayIndiciesRegular]

    # neg/neg rare case: (LagXts(xTs, base + rep(0,length(lag))) < 0) & (LagXts(xTs, base + lag) < 0)
    # bad interpretation

    # make sure one UNDERSTANDs the contexts of
    # NEG-nonlag / NEGlagged
    # use with with MUCH care

    # > sign(-4)*(-5 - (-4))/-4
    # [1] -0.25 # 25% percent less

    # > sign(-2)*(-4 - (-2))/-2
    # [1] -1 # one full proportion less
    NewCoreXts[arrayIndiciesNegNeg] <-
              sign(  zoo::coredata(lagXts(xTs, k = base + lag, ...))[arrayIndiciesNegNeg] ) *
                  (
                       zoo::coredata(xTs1)[arrayIndiciesNegNeg] -
                     ( zoo::coredata(xTs2)[arrayIndiciesNegNeg] )
                  ) /
                  zoo::coredata(xTs2)[arrayIndiciesNegNeg]

    zoo::coredata(xTs) <- NewCoreXts

  } else {
    xTs  <- xTs1/xTs2
  }

  # strait override (I know that xTs has no names)
  if(NVAR(xTs)) {
     Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"rc"),".", base, ".", lag)
  }
  xTs
})}



#' Relative Change
#'
#' @description
#' \preformatted{
#' Results may be negative.
#' }
#' @param x xts object
#' @param b choose -1 (or less) to look into the future
#' @param l observations backwards
#' @param lg log
#' @param ... dots passed
#' @return xts object
#' @examples
#' \dontrun{
#'
#' # RC (relative change) example
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#'
#' RC(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)))
#'            V1rc.0.1 V2rc.0.1
#' 1970-01-01       NA       NA
#' 1970-01-02       -2        2
#' 1970-01-03       -1        2
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo coredata
#' @export
RC <- function(x = NULL, b = 0, l = 1, lg = FALSE, ...) {
tryCatchLog::tryCatchLog({

  xTs <- relChg(x, base = b, lag = l, log = lg, ...)
  # strait override (I know that xTs has no names)
  if(NVAR(xTs)) {
     Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"rc"),".", b, ".", l)
  }
  xTs

})}




#' Absolute Relative Change
#'
#' @description
#' \preformatted{
#' Same as Relative Change.
#' Results may be negative.
#' }
#' @inheritParams RC
#' @inherit RC return details
#' @examples
#' \dontrun{
#'
#' # ARC (Absolute Relative Change) example
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#'
#' ARC(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)))
#'            V1arc.0.1 V2arc.0.1
#' 1970-01-01        NA        NA
#' 1970-01-02        -2         2
#' 1970-01-03        -1         2
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo coredata
#' @export
ARC <- function(x = NULL, b = 0, l = 1, lg = FALSE, ...) {
tryCatchLog::tryCatchLog({

  xTs <- RC(x, b = b, l = l, lg = lg, ...)
  # strait override (I know that xTs has no names)
  if(NVAR(xTs)) {
     Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"arc"),".", b, ".", l)
  }
  xTs

})}



#' Relative Relative Change
#'
#' @description
#' \preformatted{
#' Absolute value of "Absolute Relative Change"
#' Results NEVER include "negative numbers."
#' }
#' @inheritParams RC
#' @inherit RC return details
#' @examples
#' \dontrun{
#'
#' # RRC (Relative Relative Change) example
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#'
#' RRC(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)))
#'            V1rrc.0.1 V2rrc.0.1
#' 1970-01-01        NA        NA
#' 1970-01-02         2         2
#' 1970-01-03         1         2
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo coredata
#' @export
RRC <- function(x = NULL, b = 0, l = 1, lg = FALSE, ...) {
tryCatchLog::tryCatchLog({

  xTs <- abs(ARC(x, b = b, l = l, lg = lg, ...))
  # strait override (I know that xTs has no names)
  if(NVAR(xTs)) {
     Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"rrc"),".", b, ".", l)
  }
  xTs
})}








#' Lag and/or Difference/Use(rerun) a function(Fun) Upon its xts Object
#'
#' @description
#' \preformatted{
#' Based on R CRAN package xts function diff.xts
#' }
#' @param x as diff.xts
#' @param lag as diff.xts
#' @param differences as diff.xts.
#' Also, knows as "number of (re)runs - 1" of function(Fun).
#' Runs use as its input data, the results data from the the previous run (if any).s
#' @param arithmetic as diff.xts
#' @param log as diff.xts
#' @param na.pad as diff.xts
#' @param Fun difference-ing function.
#' Meant to change the xTs in some way.
#' (Default diff (expected: xts::diff.xts)).
#' Should accept or (accept and ignore) the parameters: lag;
#' for S3 compatibility, differences; for xts compatiblity,
#' arithmetic, log, and/or na.pad.
#' @param ... dots passed
#' @return xts object
#' @examples
#' \dontrun{
#'
#' # diffXts examples
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#'
#' diffXts(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)))
#'         V1diff.1 V2diff.1
#' 1970-01-01       NA       NA
#' 1970-01-02       -3        8
#' 1970-01-03       -2       16
#'
#' # same as above
#' diffXts(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)), Fun = AC)
#'            V1diff.1 V2diff.1
#' 1970-01-01       NA       NA
#' 1970-01-02       -3        8
#' 1970-01-03       -2       16
#'
#' diffXts(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)),
#'        differences = 2, Fun = AC)
#'
#'            V1diff.1 V2diff.1
#' 1970-01-01       NA       NA
#' 1970-01-02       NA       NA
#' 1970-01-03        1        8
#'
#' diffXts(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)), Fun = RC)
#'            V1diff.1 V2diff.1
#' 1970-01-01       NA       NA
#' 1970-01-02       -2        2
#' 1970-01-03       -1        2
#'
#' diffXts(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)),
#'         differences = 2, Fun = RC)
#'
#'            V1diff.1 V2diff.1
#' 1970-01-01       NA       NA
#' 1970-01-02       NA       NA
#' 1970-01-03      0.5        1
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @importFrom xts `.xts` `.index`
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
      # override
      if(NVAR(xTs)) {
         Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))), "diff"),".", init.differences)
      }
      return(xTs)
    }

  }

})}


#' Differences by using Absolute Change
#'
#' @description
#' \preformatted{
#' Differences by using Absolute Change
#' }
#' @param x xts object
#' @param l lag
#' @param d differences
#' @param ... dots passed
#' @return xts object
#' @inherit diffXts return details
#' @examples
#' \dontrun{
#'
#' # DFA(Differences by using Absolute Change) examples
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#'
#' DFA(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)))
#'            V1dfa.2.1 V2dfa.2.1
#' 1970-01-01        NA        NA
#' 1970-01-02        -3         8
#' 1970-01-03        -2        16
#'
#' DFA(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)), d = 2)
#'            V1dfa.2.1 V2dfa.2.1
#' 1970-01-01        NA        NA
#' 1970-01-02        NA        NA
#' 1970-01-03         1         8
#'
#' DFA(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)), l = 2)
#'            V1dfa.2.1 V2dfa.2.1
#' 1970-01-01        NA        NA
#' 1970-01-02        NA        NA
#' 1970-01-03        -5        24
#' }
#' @export
DFA <- function(x, l=1, d=1, ...) {
tryCatchLog::tryCatchLog({

  xTs <- diffXts(x, lag=l, differences=d, arithmetic=TRUE, log=FALSE, na.pad=TRUE, Fun = AC, ...)
  # override
  if(NVAR(xTs)) {
     Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"dfa"),".", l, ".", d)
  }
  xTs

})}



#' Differences by using Relative Change
#'
#' @description
#' \preformatted{
#' Differences by using Relative Change
#' }
#' @param x xts object
#' @param l lag
#' @param d differences
#' @param ... dots passed
#' @return xts object
#' @inherit diffXts return details
#' @examples
#' \dontrun{
#'
#' # DFR(Differences by using Relative Change) examples
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#'
#' DFR(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)))
#'            V1dfr.2.1 V2dfr.2.1
#' 1970-01-01        NA        NA
#' 1970-01-02        -2         2
#' 1970-01-03        -1         2
#'
#' DFR(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)), d = 2)
#'            V1dfr.2.1 V2dfr.2.1
#' 1970-01-01        NA        NA
#' 1970-01-02        NA        NA
#' 1970-01-03       0.5         1
#'
#' DFR(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)), l = 2)
#'            V1dfr.2.1 V2dfr.2.1
#' 1970-01-01        NA        NA
#' 1970-01-02        NA        NA
#' 1970-01-03        -4         4
#' }
#' @export
DFR <- function(x, l=1, d=1, ...) {
tryCatchLog::tryCatchLog({

  xTs <- diffXts(x, lag=l, differences=d, arithmetic=TRUE, log=FALSE, na.pad=TRUE, Fun = RC, ...)
  # override
  if(NVAR(xTs)) {
     Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"dfr"),".", l, ".", d)
  }
  xTs
})}




#' rolling ranks using TTR::runPercentRank
#'
#' @description
#' This is a wrapper around the R CRAN package TTR function runPercentRank.
#' TTR runPercentRank gives skewed values
#' (but with the value are in the correct order).
#' This function uses that "proper ordering" and makes
#' usable running ranks.
#'
#' @param x xts object
#' @param window 10(default) lag to determine the ranks.
#' If cumulative=TRUE, the number of observations to use
#' before the first result is returned. Not tested. So beware.
#' Must be between 1 and nrow(x), inclusive
#' @param ranks window(default) number of ranks.
#' @param cumulative FALSE(default) use from-inception calculation?
#' Not tested. So beware.
#' @param exact.multiplier The weight applied to identical values
#' in the window. Must be between 0 and 1, inclusive.
#' See ? TTR::runPercentRank
#' @param ... dots passed
#' @return xts object. Lower x coredata values means lower rank numbers..
#' @references
#'\cite{last Fortran version of percentRank.f (but the newer C version has a fix)
#'\url{https://github.com/joshuaulrich/TTR/blob/9b30395f7604c37ea12a865961d81666bc167616/src/percentRank.f}
#'}
#' @examples
#' \dontrun{
#'
#' # runRanks(rolling ranks using TTR::runPercentRank) examples
#'
#' runRanks(xts(c(3, 1, 2, 3), zoo::as.Date(0:3)), window = 2)
#'            V1rnk.2.2
#' 1970-01-01        NA
#' 1970-01-02         1
#' 1970-01-03         2
#' 1970-01-04         2
#'
#' runRanks(xts(c(3, 1, 2, 3), zoo::as.Date(0:3)), window = 3, ranks = 2)
#'            V1rnk.3.2
#' 1970-01-01        NA
#' 1970-01-02        NA
#' 1970-01-03         1
#' 1970-01-04         2
#'
#' # the window is larger than the data
#' runRanks(xts(c(3, 1, 2, 3), zoo::as.Date(0:3)), window = 5)
#'            V1rnk.5.5
#' 1970-01-01        NA
#' 1970-01-02        NA
#' 1970-01-03        NA
#' 1970-01-04        NA
#'}
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom TTR runPercentRank
#' @export
runRanks <- function(x, window = 10, ranks = window, cumulative = F, exact.multiplier = 0.5, ...) {
tryCatchLog::tryCatchLog({

  if(NVAR(x) > 1) {
     stop("TTR runPercentRank ignores all other columns")
  }

  if(ranks <= 1 || window <= 1) {
    stop(paste0("\"windows\" and \"ranks\" must be greater than one(1)"))
  }

  if(window < ranks) {
    stop("if parameters: window < ranks;  TTR runPercentRank gives un-interpretable(not-useful) results")
  }

  xTs <- x

  if(window <= NROW(xTs)) {
    # number between zero(0) and one(1)
           # internally
           # x <- try.xts(x, error = as.matrix)
    xTs <- TTR::runPercentRank(xTs, n = window, cumulative = cumulative, exact.multiplier = exact.multiplier)
           # number between zero(0) and "rank"
    xTs <- xTs * ranks                                    # very important
                          # splitter sequence # 1, 2, ... rank-1
    zoo::coredata(xTs) <- findInterval(zoo::coredata(xTs), vec = { seq_len(ranks - 1) }, left.open = TRUE) + 1
  } else {
    xTs <- xts(rep(NA_real_, NROW(xTs)), zoo::index(xTs))
  }
  # override
  if(NVAR(xTs)) {
     Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"rnk"),".",  window, ".", ranks)
  }
  xTs
})}



#' rolling ranks using TTR::runPercentRank
#'
#' @description
#' This is a wrapper around the R CRAN package TTR function runPercentRank.
#' TTR runPercentRank gives skewed values
#' (but with the value are in the correct order).
#' This function uses that "proper ordering" and makes
#' usable running ranks.
#'
#' @param x xts object
#' @param w window 10(default) lag to determine the ranks.
#' If cumulative=TRUE, the number of observations to use
#' before the first result is returned. Not tested. So beware.
#' Must be between 1 and nrow(x), inclusive
#' @param r ranks window(default) number of ranks.
#' @param ... dots passed
#' @return xts object. Lower x coredata values means lower rank numbers.
#' @references
#'\cite{last Fortran version of percentRank.f (but the newer C version has a fix)
#'\url{https://github.com/joshuaulrich/TTR/blob/9b30395f7604c37ea12a865961d81666bc167616/src/percentRank.f}
#'}
#' @examples
#' \dontrun{
#'
#' # RNK(rolling ranks using TTR::runPercentRank) examples
#'
#' RNK(xts(c(3, 1, 2, 3), zoo::as.Date(0:3)), w = 2)
#'            V1rnk.2.2
#' 1970-01-01        NA
#' 1970-01-02         1
#' 1970-01-03         2
#' 1970-01-04         2
#'
#' RNK(xts(c(3, 1, 2, 3), zoo::as.Date(0:3)), w = 3, r = 2)
#'            V1rnk.3.2
#' 1970-01-01        NA
#' 1970-01-02        NA
#' 1970-01-03         1
#' 1970-01-04         2
#'
#' # the window is larger than the data
#' RNK(xts(c(3, 1, 2, 3), zoo::as.Date(0:3)), w = 5)
#'            V1rnk.5.5
#' 1970-01-01        NA
#' 1970-01-02        NA
#' 1970-01-03        NA
#' 1970-01-04        NA
#'}
#' @importFrom tryCatchLog tryCatchLog
#' @export
RNK <- function(x, w = 10, r = w, ...) {
tryCatchLog::tryCatchLog({

  xTs <- runRanks(x, window = w, ranks = w, ...)
  # override
  if(NVAR(xTs)) {
     Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"rnk"),".",  w, ".", r)
  }
  xTs
})}

