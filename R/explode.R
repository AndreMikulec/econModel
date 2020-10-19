









#' Interleave two objects of arbitrary length
#'
#' @description
#' \preformatted{
#'
#' from R CRAN package rmngb
#' }
#' @param x index-able objet by `[` and the same index length as y.  Compatible type with y and combinable(c)
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



#' pairwise interleave of two two dimensional index-able objects
#'
#' @description
#' \preformatted{
#'
#' If one or the other has one index element while the other
#' has N index elements, then the one will be recycled to N index elements.
#'
#' }
#' @param x two dimension index-able object
#' @param y two dimension index-able object
#' @return list of length two of two interleaved two dimensional index-able  objects
#' @examples
#' \dontrun{
#'#
#'  list(iris[1:2,1:2], airquality[1:2,1:2])
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
#'  str( pairWise( iris[1:2,1:2], airquality[1:2,1:2] ) )
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
#'   str( pairWise( iris[1:2,1:2], airquality[1:2,1, drop = F] ) )
#'#  List of 2
#'#   $ :List of 2
#'#    ..$ Sepal.Length: num [1:2] 5.1 4.9
#'#    ..$ Ozone       : int [1:2] 41 36
#'#   $ :List of 2
#'#    ..$ Sepal.Width: num [1:2] 3.5 3
#'#    ..$ Ozone      : int [1:2] 41 36
#'#
#'  library(xts)
#'  data("sample_matrix", package = "xts")
#'  str( pairWise(as.xts(sample_matrix)[,1:2], as.xts(sample_matrix)[,3:4] ) )
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
#'  library(quantmod)
#'  ibm <- quantmod::getSymbols("IBM", from = "1970-01-01", to = "1970-01-13", auto.assign = FALSE)
#'  pairWise(tail(ibm[,c("IBM.Open","IBM.Close")]), xts(, zoo::as.Date(0)[0]))
#'
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
#'#   integer(0)
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
#'#   integer(0)
#'#
#'  pairWise(tail(ibm[,c("IBM.Open")]), xts())
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
#'#   integer(0)
#'#
#'#
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
pairWise <- function(x, y) {

  # recycling 1 to N recycling
  #
  RepDone <- FALSE
  if(!RepDone && NVAR(x) != NVAR(y) && NVAR(x) > 0 && NVAR(y) == 1) {

    y <- DescTools::DoCall(cbind, rep(list(y),NVAR(x)))
    Names(y) <- rep(Names(y)[1], NVAR(y))
    RepDone <- TRUE
  }
  if(!RepDone && NVAR(x) != NVAR(y) && NVAR(y) > 0 && NVAR(x) == 1) {

    x <- DescTools::DoCall(cbind, rep(list(x),NVAR(y)))
    Names(x) <- rep(Names(x)[1], NVAR(x))
    RepDone <- TRUE
  }

  if(!RepDone && NVAR(x) != NVAR(y) && (NVAR(y) == 0 || NVAR(x) == 0)) {

    if(!RepDone && NVAR(x) == 0 && NVAR(y) > 0) {
      x <- rep(list(x), NVAR(y))
      RepDone <- TRUE
    }

    if(!RepDone && NVAR(x) > 0  && NVAR(y) == 0) {
      y <- rep(list(y), NVAR(x))
      RepDone <- TRUE
    }

  }

  List <- c(list(),lapply(x, identity), lapply(y, identity))

  if(length(List) > 2 ) { # e.g. 4, 6, 8, ...
    L1coord <- seq(from = 1, by = 2, length.out = 0.5*length(List))
    L2coord <- seq(from = 2, by = 2, length.out = 0.5*length(List))
    res <- c(list(List[L1coord]),list(List[L2coord]))
  } else { # i.e. 2
    res <- list(List)
  }

  return(res)

}




#' generate a good column name
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param x object with the old column names
#' @return object with the old column names
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @importFrom stringr str_replace_all
newXtsColName <- function(xTs = NULL, Fun =  NULL, isCharFun = NULL, xTs1 = NULL, xTs2 = NULL, WhichCombo =  NULL, AltName = NULL, Prefix = NULL, FixedSep = NULL) {
tryCatchLog::tryCatchLog({


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

  paste0(
    c(
      if(!is.null(xTs1) && (NCOL(xTs1) > 0)) { colnames(xTs1)[1] } else { NULL },
      if(!is.null(xTs2) && (NCOL(xTs2) > 0)) { colnames(xTs2)[1] } else { NULL }
    ), collapse = FixedSep) -> Colnames

  if(length(WhichCombo)) {
    WhichCombo <-  paste0(c(interleave(names(WhichCombo), unlist(WhichCombo))), collapse = FixedSep)
  } else {
    WhichCombo <- NULL
  }

  PreName <- NULL; PostName <- NULL
  NewNameWhichCombo <- paste0(c(NewName, WhichCombo), collapse = FixedSep)
  if(is.null(Prefix) || (Prefix == FALSE)) {
    PostName <- NewNameWhichCombo
  } else {
    PreName  <- NewNameWhichCombo
  }
  NewName <- paste0(c(PreName, Colnames, PostName), collapse = FixedSep)
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


  xTs1  <- as.xts(xTs1)
  if(!is.null(xTs2)) xTs2 <- as.xts(xTs2)
  if(is.null(FixedSep)) FixedSep = "."

  DescTools::DoCall(expand.grid, Whiches) %>%
    as.list %>%
    { purrr::transpose(.) } -> WhichesCombinations
  if(!NCOL(WhichesCombinations)){ return(xts()) }

  if(mode(Fun) == "function") {
    Fun = match.fun(Fun)
    isCharFun <- FALSE
  } else {
    isCharFun <- TRUE
  }

  xTs <- xts()
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
