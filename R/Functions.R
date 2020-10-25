
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
INA <- function(x) {
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




#' Relative Change
#'
#' @description
#' \preformatted{
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
#' @importFrom xts `.xts`
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
         Names(xTs) <- paste0(paste0(paste0(rep("V",NVAR(xTs)),seq(1,NVAR(xTs))),"diff"),".", init.differences)
      }
      return(xTs)
    }

  }

})}


#' Differences by using Absolute Change
#'
#' @description
#' \preformatted{
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




