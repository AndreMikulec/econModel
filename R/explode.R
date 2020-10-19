

#' sets the enviroment
#'
#' @description
#' \preformatted{
#'
#' This is a space-saver that is meant to be used at the beginning of a function.
#'
#' Variable ops from the calling environment sets R options.
#' Environment variable TZ using the calling environment variable oldtz sets.
#'
#' }
#'
#' @return envi, options, and TZ are set
#' @examples
#' \dontrun{
#' # > options(max.print=88888L)
#' # initEnv()
#' # > getOption("max.print")
#' # [1] 99999
#' }
#' @export
#' @importFrom futile.logger flog.threshold ERROR flog.appender appender.tee
#' @importFrom logging basicConfig
#' @importFrom rlang parse_expr eval_bare caller_env
#' @importFrom magrittr %>%
#' @importFrom tryCatchLog tryCatchLog
initEnv <- function(init = NULL) {
  # tryCatchLog: what level to activate
  futile.logger::flog.threshold(futile.logger::ERROR)
  futile.logger::flog.appender(futile.logger::appender.tee("tryCatchLog.logged.txt"))
  logging::basicConfig()
  tryCatchLog::tryCatchLog({
    # so I have one
    action <- rlang::parse_expr("assign(\"envi\", environment())")
    envii  <- rlang::caller_env()
    rlang::eval_bare(action, env = envii)

    # LATER MOVE THIS DOWN INTO FUNCTIONS
    assign("%>%",  magrittr::`%>%` , envir = envii)

    # LATER MOVE THESE DOWN INTO FUNCTIONS
    if(!"quantmod" %in% search())                 require(quantmod)
    if(!"PerformanceAnalytics" %in% search())     require(PerformanceAnalytics)

    # TOO MUCH INFORMATION ( BUT DOES WORK )
    # action <- rlang::parse_expr("assign(\"ssc\", toString(sys.calls()[[length(sys.calls())]]) )")
    # rlang::eval_bare(action, env = envii)
    # action <- rlang::parse_expr("print(ssc)")
    # rlang::eval_bare(action, env = envii)

    # options(warn=2L)
    options(warn=1L) # 1L # so, I can use BELOW: options(tryCatchLog.write.error.dump.file = TRUE)
    options(width=10000L) # LIMIT # Note: set Rterm(64 bit) as appropriate
    # options(digits=if(is.null(init[["digits"]])) { 22L } else {init[["digits"]]})
    options(digits = 5L)
    options(max.print=99999L)
    options(scipen=255L) # Try these = width

    # BECAUSE MY "WARNINGS ARE CONVERTED TO ERRORS" ( ABOVE: options(warn=2L) )
    # I can not directly use this feature
    # Error in save(list = names(.GlobalEnv), file = outfile, version = version,  :
    # (converted from warning) 'package:stats' may not be available when loading
    options(tryCatchLog.write.error.dump.file = TRUE)

    # ops <- options()
    # # pre-save options to ignore
    # ops <- ops[!names(ops) %in% ignore_ops]
    # options(ops)
    # assign("ops", ops, envir = envii)

    # pre-save options to not-ignore
    # ops_not_ignored <- options()[!names(options()) %in% ignore_ops]
    # assign("ops", options(ops_not_ignored), envir = envii)

    ops <- options()
    options(ops)
    assign("ops", ops, envir = envii)

    #correct for TZ
    oldtz <- Sys.getenv("TZ")
    if(oldtz!="UTC") {
      Sys.setenv(TZ="UTC")
    }
    #
    assign("oldtz", oldtz, envir = envii)

    invisible()

})}




#' unsets the enviroment
#'
#' @description
#' \preformatted{
#'
#' This is a space-saver that is meant to be used at the beginning of a function.
#'
#' Variable ops from the calling environment resets R options.
#' Environment variable TZ using the calling environment variable oldtz resets.
#'
#' }
#'
#' @return options, and TZ are un-set
#' @examples
#' \dontrun{
#' # > uninitEnv()
#' # getOption("digits")
#' # [1] 5
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom rlang caller_env
uninitEnv <- function() {
tryCatchLog::tryCatchLog({

  # eventually calling envir will NOT BE the lexically calling environment
  envii <- rlang::caller_env()
  Sys.setenv(TZ=get("oldtz", envir = envii))

  # ops <- get("ops", envir = envii)
  # # post-save options to ignore
  # ops <- ops[!names(ops) %in% ignore_ops]
  # options(ops)

  # # post-save options to ignore
  # ops_ignored <- options()[names(options()) %in% ignore_ops]
  #
  # ops_temp <- get("ops", envir = envii)
  # # remove
  # ops <- ops_temp[!names(ops_temp)  %in% ignore_ops]
  # options(ops)
  #
  # # add back
  # options(ops_ignored)

  ops <- get("ops", envir = envii)
  options(ops)

  invisible()

})}




#' garantee a passed xts object or a zero length xts object
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object
#' @return xts object
#' @examples
#' \dontrun{
#' # > initXts(xTs = NULL)
#' # Data:
#' # numeric(0)
#' #
#' # Index:
#' # Date of length 0
#' #
#' # > initXts(zoo::as.Date(0)[0])
#' # Data:
#' # numeric(0)
#' #
#' # Index:
#' # Date of length 0
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo as.Date
initXts <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  if(is.null(xTs)) {
    # empty xts
    xTs <-  xts(, zoo::as.Date(0)[0])
  } else if (is.timeBased(xTs)) {
    xTs <-  xts(, xTs)
  } else {}
  xTs

})}




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
#'
#' @param x object
#' @return integer number of variables
#' @rdname NVAR
#' @export
NVAR <- function(x = NULL) {
  # tryCatchLog is not allowed here
  UseMethod("NVAR")
}



#' @rdname NVAR
#' @export
#' @importFrom tryCatchLog tryCatchLog
NVAR.default <- function(x = NULL) {
tryCatchLog::tryCatchLog({

  if(is.null(x)) return(0L)
  NCOL(x)

})}



#' @rdname NVAR
#' @examples
#' \dontrun{
#' # > require(xts)
#' # > NVAR(xts(, zoo::as.Date("1970-01-12")))
#' # [1] 0
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
NVAR.xts <- function(x = NULL) {
tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  x <- initXts(x)

  if(length(coredata(x))) {
    res <- NCOL(coredata(x))
  } else {
    res <- 0L
  }
  return(res)

})}



#' Interleave two objects of arbitrary length
#'
#' @description
#' \preformatted{
#'
#' from R CRAN package rmngb
#' }
#' @parm x index-able objet by `[` and the same index length as y.  Compatible type with y and combinable(c)
#' @param y index-able object by `[` and the same index length as x. Compatible type with x and combinable(c)
#' @return new combined object
#'
#' @export
interleave <- function (x, y)
{
  iX <- 2 * seq_along(x) - 1
  iY <- 2 * seq_along(y)
  c(x, y)[order(c(iX, iY))]
}



#' pairwise interleave of index-able objects
#'
#' @description
#' \preformatted{
#'
#' This works better on xts objects.
#' (lapply or plyr::llply structure is held together.)
#'
#' If one or the other has one index element while the other
#' has N index elements, then the one will be recycled to N index elements.
#'
#' Helper to eXplode.
#'
#' }
#'
#' @param x1 data.frame or xts object
#' @param x2 data.frame or xts object
#' @return list of length two of two data.frames or xts objects
#' @examples
#' \dontrun{
#'#
#'# list(iris[1:2,1:2], airquality[1:2,1:2])
#'# [[1]]
#'#   Sepal.Length Sepal.Width
#'# 1          5.1         3.5
#'# 2          4.9         3.0
#'#
#'# [[2]]
#'#   Ozone Solar.R
#'# 1    41     190
#'# 2    36     118
#'#
#'# > str( pairWise( iris[1:2,1:2], airquality[1:2,1:2] ) )
#'# List of 2
#'#  $ :List of 2
#'#   ..$ Sepal.Length: num [1:2] 5.1 4.9
#'#   ..$ Ozone       : int [1:2] 41 36
#'#  $ :List of 2
#'#   ..$ Sepal.Width: num [1:2] 3.5 3
#'#   ..$ Solar.R    : int [1:2] 190 118
#'#
#'# # 1 by N recycling
#'# #
#'#  > str( pairWise( iris[1:2,1:2], airquality[1:2,1, drop = F] ) )
#'#  List of 2
#'#   $ :List of 2
#'#    ..$ Sepal.Length: num [1:2] 5.1 4.9
#'#    ..$ Ozone       : int [1:2] 41 36
#'#   $ :List of 2
#'#    ..$ Sepal.Width: num [1:2] 3.5 3
#'#    ..$ Ozone      : int [1:2] 41 36
#'#
#'# > require(xts)
#'# > data("sample_matrix", package = "xts")
#'# > str( pairWise(as.xts(sample_matrix)[,1:2], as.xts(sample_matrix)[,3:4] ) )
#'# List of 2
#'#  $ :List of 2
#'#   ..$ Open:An 'xts' object on 2007-01-02/2007-06-30 containing:
#'#   Data: num [1:180, 1] 50 50.2 50.4 50.4 50.2 ...
#'#  - attr(*, "dimnames")=List of 2
#'#   ..$ : NULL
#'#   ..$ : chr "Open"
#'#   Indexed by objects of class: [POSIXct,POSIXt] TZ:
#'#   xts Attributes:
#'#  NULL
#'#   ..$ Low :An 'xts' object on 2007-01-02/2007-06-30 containing:
#'#   Data: num [1:180, 1] 50 50.2 50.3 50.2 50.1 ...
#'#  - attr(*, "dimnames")=List of 2
#'#   ..$ : NULL
#'#   ..$ : chr "Low"
#'#   Indexed by objects of class: [POSIXct,POSIXt] TZ:
#'#   xts Attributes:
#'#  NULL
#'#  $ :List of 2
#'#   ..$ High :An 'xts' object on 2007-01-02/2007-06-30 containing:
#'#   Data: num [1:180, 1] 50.1 50.4 50.4 50.4 50.2 ...
#'#  - attr(*, "dimnames")=List of 2
#'#   ..$ : NULL
#'#   ..$ : chr "High"
#'#   Indexed by objects of class: [POSIXct,POSIXt] TZ:
#'#   xts Attributes:
#'#  NULL
#'#   ..$ Close:An 'xts' object on 2007-01-02/2007-06-30 containing:
#'#   Data: num [1:180, 1] 50.1 50.4 50.3 50.3 50.2 ...
#'#  - attr(*, "dimnames")=List of 2
#'#   ..$ : NULL
#'#   ..$ : chr "Close"
#'#   Indexed by objects of class: [POSIXct,POSIXt] TZ:
#'#   xts Attributes:
#'#  NULL
#'#
#'# > ibm <- quantmod::getSymbols("IBM", from = "1970-01-01", to = "1970-01-13", auto.assign = FALSE)
#'#
#'# > pairWise(tail(ibm[,c("IBM.Open","IBM.Close")]), xts(, zoo::as.Date(0)[0]))
#'# [[1]]
#'# [[1]]$IBM.Open
#'# IBM.Open
#'# 1970-01-05   18.300
#'# 1970-01-06   18.413
#'# 1970-01-07   18.425
#'# 1970-01-08   18.438
#'# 1970-01-09   18.475
#'# 1970-01-12   18.450
#'#
#'# [[1]][[2]]
#'# Data:
#'#   numeric(0)
#'#
#'# Index:
#'#   Date of length 0
#'#
#'#
#'# [[2]]
#'# [[2]]$IBM.Close
#'# IBM.Close
#'# 1970-01-05    18.413
#'# 1970-01-06    18.425
#'# 1970-01-07    18.438
#'# 1970-01-08    18.475
#'# 1970-01-09    18.450
#'# 1970-01-12    18.388
#'#
#'# [[2]][[2]]
#'# Data:
#'#   numeric(0)
#'#
#'# Index:
#'#   Date of length 0
#'#
#'# > pairWise(tail(ibm[,c("IBM.Open")]), initXts())
#'# [[1]]
#'# [[1]]$IBM.Open
#'# IBM.Open
#'# 1970-01-05   18.300
#'# 1970-01-06   18.413
#'# 1970-01-07   18.425
#'# 1970-01-08   18.438
#'# 1970-01-09   18.475
#'# 1970-01-12   18.450
#'#
#'# [[1]][[2]]
#'# Data:
#'#   numeric(0)
#'#
#'# Index:
#'#   Date of length 0
#'#
#'#
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @importFrom plyr llply
pairWise <- function(x1, x2) {

  # # recycling 1 to N recycling
  # #
  # if((NVAR(x1) != NVAR(x2)) && NVAR(x2) == 1) {
  #
  #   x2 <- DescTools::DoCall(cbind, rep(list(x2),NVAR(x1)))
  #   Names(x2) <- rep(Names(x2)[1], NVAR(x2))
  #
  # }
  # if((NVAR(x1) != NVAR(x2)) && NVAR(x1) == 1) {
  #
  #   x1 <- DescTools::DoCall(cbind, rep(list(x1),NVAR(x2)))
  #   Names(x1) <- rep(Names(x1)[1], NVAR(x1))
  # }

  # recycling 1 to N recycling
  #
  RepDone <- FALSE
  if(!RepDone && NVAR(x1) != NVAR(x2) && NVAR(x1) > 0 && NVAR(x2) == 1) {

    x2 <- DescTools::DoCall(cbind, rep(list(x2),NVAR(x1)))
    Names(x2) <- rep(Names(x2)[1], NVAR(x2))
    RepDone <- TRUE
  }
  if(!RepDone && NVAR(x1) != NVAR(x2) && NVAR(x2) > 0 && NVAR(x1) == 1) {

    x1 <- DescTools::DoCall(cbind, rep(list(x1),NVAR(x2)))
    Names(x1) <- rep(Names(x1)[1], NVAR(x1))
    RepDone <- TRUE
  }
  #
  # if(!RepDone && NVAR(x1) != NVAR(x2) && (NVAR(x2) == 0 || NVAR(x1) == 0)) {
  #
  #   if(NVAR(x1) == 0) {
  #     x1 <- NULL
  #   }
  #
  #   if(NVAR(x2) == 0) {
  #     x2 <- NULL
  #   }
  #
  #   RepDone <- TRUE
  # }


  if(!RepDone && NVAR(x1) != NVAR(x2) && (NVAR(x2) == 0 || NVAR(x1) == 0)) {

    if(!RepDone && NVAR(x1) == 0 && NVAR(x2) > 0) {
      x1 <- rep(list(x1), NVAR(x2))
      RepDone <- TRUE
    }

    if(!RepDone && NVAR(x1) > 0  && NVAR(x2) == 0) {
      x2 <- rep(list(x2), NVAR(x1))
      RepDone <- TRUE
    }

  }

  List <- c(list(),plyr::llply(x1, identity), plyr::llply(x2, identity))

  if(length(List) > 2 ) { # e.g. 4, 6, 8, ...
    L1coord <- seq(from = 1, by = 2, length.out = 0.5*length(List))
    L2coord <- seq(from = 2, by = 2, length.out = 0.5*length(List))
    res <- c(list(List[L1coord]),list(List[L2coord]))
  } else { # i.e. 2
    res <- list(List)
  }

  return(res)

}




#' generate a good xts column name
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param x single column xts with the old column name
#' @return single column xts with  the new column name
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @importFrom stringr str_replace_all
newXtsColName <- function(xTs = NULL, Fun =  NULL, isCharFun = NULL, xTs1 = NULL, xTs2 = NULL, WhichCombo =  NULL, AltName = NULL, Prefix = NULL, FixedSep = NULL) {
tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  if(is.null(isCharFun)) stop("newXtsColName need actual paramter isCharFun")

  if(is.null(AltName)) {
    if(isCharFun) {
      NewName <- stringr::str_replace_all(Fun,"[.]|::",FixedSep)
    } else {
      NewName <- "anon"
    }
  } else {
    NewName <- AltName
  }

  stringr::str_c(
    c(
      if(!is.null(xTs1) && (NCOL(xTs1) > 0)) { colnames(xTs1)[1] } else { NULL },
      if(!is.null(xTs2) && (NCOL(xTs2) > 0)) { colnames(xTs2)[1] } else { NULL }
    ), collapse = FixedSep) -> Colnames

  if(length(WhichCombo)) {
    WhichCombo <-  stringr::str_c(c(interleave(names(WhichCombo), unlist(WhichCombo))), collapse = FixedSep)
  } else {
    WhichCombo <- NULL
  }

  PreName <- NULL; PostName <- NULL
  NewNameWhichCombo <- stringr::str_c(c(NewName, WhichCombo), collapse = FixedSep)
  if(is.null(Prefix) || (Prefix == FALSE)) {
    PostName <- NewNameWhichCombo
  } else {
    PreName  <- NewNameWhichCombo
  }
  NewName <- stringr::str_c(c(PreName, Colnames, PostName), collapse = FixedSep)
  colnames(xTs)[1] <-NewName

  xTs

})}



#' expland out xts
#'
#' @description
#' \preformatted{
#'
#' from an xts function stub, create an xts object of derived columns
#'
#' Meant to create many package TTR/PerformanceAnalytics column results
#'
#' Idea was from
#'
#' Time series cross-validation 5
#' January 24, 2013
#' By Deane-Mayer
#' http://www.r-bloggers.com/time-series-cross-validation-5/
#' http://moderntoolmaking.blogspot.com/2013/01
#'       /time-series-cross-validation-5.html?utm_source=feedburner
#'       &utm_medium=feed&utm_campaign=Feed%3A+ModernToolMaking+%28Modern+Tool+Making%29
#'
#' NOTE: if any xTs2, then xTs1 and xTs2 are paired/matched column position to column position.
#'
#' }
#'
#' @param xTs1 xts object
#' @param xTs2 xts object
#' @param Fun function name in the "bare" or in literal quotes("")
#' @param Whiches list of possible varying parameters that are expanded
#' to all possible combinations by expand.grid
#' @param AltName string alternate name for "Fun"
#' @param Prefix boolan default is FALSE.  TRUE would place the column meta before the column name.
#' @param FixedSep string divider of meta items
#' @param quote boolean passed to DescTools DoCall
#' @param envir calling environment
#' @return new xts object of new derived columns
#' @examples
#' \dontrun{
#'
#' # require(quantmod)
#' # ibm <- getSymbols("IBM", from = "1970-01-01", to = "1970-01-13", auto.assign = FALSE)
#' # explodeXts(ibm[,c("IBM.Open","IBM.Close")], Fun = "TTR::SMA", Whiches = list(n = 2:3))
#' #
#' #            IBM.Open.TTR.SMA.n.2 IBM.Close.TTR.SMA.n.2 IBM.Open.TTR.SMA.n.3 IBM.Close.TTR.SMA.n.3
#' # 1970-01-02                   NA                    NA                   NA                    NA
#' # 1970-01-05               18.262                18.325                   NA                    NA
#' # 1970-01-06               18.356                18.419               18.312                18.358
#' # 1970-01-07               18.419                18.431               18.379                18.425
#' # 1970-01-08               18.431                18.456               18.425                18.446
#' # 1970-01-09               18.456                18.463               18.446                18.454
#' # 1970-01-12               18.463                18.419               18.454                18.438
#'
#' # > explodeXts(ibm[,c("IBM.Open")], Fun = "TTR::SMA", Whiches = list(n = 2:3))
#' # IBM.Open.TTR.SMA.n.2 IBM.Open.TTR.SMA.n.3
#' # 1970-01-02                   NA                   NA
#' # 1970-01-05               18.262                   NA
#' # 1970-01-06               18.356               18.312
#' # 1970-01-07               18.419               18.379
#' # 1970-01-08               18.431               18.425
#' # 1970-01-09               18.456               18.446
#' # 1970-01-12               18.463               18.454
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom purrr transpose
#' @importFrom plyr llply
#' @importFrom DescTools DoCall
#' @export
explodeXts <- function(  xTs1 = NULL, xTs2 = NULL, Fun = NULL
                         , Whiches   = NULL
                         , AltName   = NULL, Prefix = NULL, FixedSep  = NULL
                         , quote     = FALSE, envir = parent.frame(2)
                         , ...){
tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  xTs1  <- initXts(xTs1)
  xTs2  <- initXts(xTs2)
  if(is.null(FixedSep)) FixedSep = "."

  DescTools::DoCall(expand.grid, Whiches) %>%
    as.list %>%
    { purrr::transpose(.) } -> WhichesCombinations
  if(!NCOL(WhichesCombinations)){ return(initXts()) }

  if(mode(Fun) == "function") {
    Fun = match.fun(Fun)
    isCharFun <- FALSE
  } else {
    isCharFun <- TRUE
  }

  xTs <- initXts()
  FunctionEnv <- environment()

  plyr::llply(WhichesCombinations, function(WhichCombo) {

    plyr::llply(pairWise(xTs1, xTs2), function(xTsColumnSet) {

      xTs1 <- xTsColumnSet[[1]]
      xTs2 <- xTsColumnSet[[2]]
      # will no longer happen: pairWise always returns a 'list of pairs'
      # if(length(xTsColumnSet) >= 2) { xTs2 <- xTsColumnSet[[2]] } else { xTs2 <- NULL }

      if(NVAR(xTs2)) { xTs2List <- list(xTs2) } else { xTs2List <- NULL }
      Temp <- DescTools::DoCall(Fun, args = c(list(), list(xTs1), xTs2List, WhichCombo, list(...)), quote = quote, envir = envir)

      Temp <- newXtsColName( Temp, Fun = Fun, isCharFun = isCharFun, xTs1 = xTs1, xTs2 = xTs2, WhichCombo = WhichCombo
                             , AltName = AltName, Prefix = Prefix, FixedSep = FixedSep)

      assign("xTs", merge(xTs, Temp), envir = FunctionEnv)

      invisible()

    }) -> Empty

  }) -> Empty

  xTs

})}
