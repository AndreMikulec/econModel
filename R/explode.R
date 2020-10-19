


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
#' Given column names of one or two two dimensional objects,
#' each with just one single column,  and a function and its arguments,
#' generate a new column name and return the original 'one colun' two dimensional object
#'
#' }
#'
#' @param x object with the old column names
#' @param Fun literally written function or the function name inside a string
#' @param isCharFun hint as to Fun being a function or the function name inside of a string
#' @param x1 object providing part of the new colum name
#' @param x2 object providing (yet another) part of the the new column name
#' @param FlagsCombo list with named elements and their values to (eventually) become part of the new column name
#' @param AltName column new root name.  Default is NULL. Unless a string is provided then the root name will not be replaced.
#' @param Prefix place the new addition to the column name at the front instead of the end. Default is NULL. Internally the default is FALSE.
#' @param FixedSep replacement for "[.]|::" that was found in the function name
#' @return object with the new column names
#' @export
#' @importFrom tryCatchLog tryCatchLog
newColName <- function(x = NULL, Fun =  NULL, isCharFun = NULL, x1 = NULL, x2 = NULL, FlagsCombo =  NULL, AltName = NULL, Prefix = NULL, FixedSep = NULL) {
tryCatchLog::tryCatchLog({

  if(is.null(isCharFun)) stop("newColNames need actual paramter isCharFun")

  if(is.null(AltName)) {
    if(isCharFun) {
      NewName <- gsub("[.]|::", FixedSep, Fun)
    } else {
      NewName <- "Anon"
    }
  } else {
    NewName <- AltName
  }

  paste0(
    c(
      if(!is.null(x1) && (NCOL(x1) > 0)) { colnames(x1)[1] } else { NULL },
      if(!is.null(x2) && (NCOL(x2) > 0)) { colnames(x2)[1] } else { NULL }
    ), collapse = FixedSep) -> Colnames

  if(length(FlagsCombo)) {
    FlagsCombo <-  paste0(c(interleave(names(FlagsCombo), unlist(FlagsCombo))), collapse = FixedSep)
  } else {
    FlagsCombo <- NULL
  }

  PreName <- NULL; PostName <- NULL
  NewNameWhichCombo <- paste0(c(NewName, FlagsCombo), collapse = FixedSep)
  if(is.null(Prefix) || (Prefix == FALSE)) {
    PostName <- NewNameWhichCombo
  } else {
    PreName  <- NewNameWhichCombo
  }
  NewName <- paste0(c(PreName, Colnames, PostName), collapse = FixedSep)
  colnames(x)[1] <-NewName

  x

})}



#' expland out a two dimensional object
#'
#' From one(1) or two(2) two dimension objects, apply a function(3) upon the two objects.
#' Return a two dimensional object of with a column name derived from the three.
#' Functions are meant to be many of the functions from R CRAN packages package TTR and PerformanceAnalytics.
#' Note, if any x2, then x1 and x2 are paired/matched column position to column position.
#' Note, this the "multivariate form" of the "single variate form" object TTR function data generator from the web page "Time series cross-validation 5".
#'
#' @param x1 two dimensional object
#' @param x2 Optionally, second two dimensional object.
#' @param Fun function name in the "bare" or in literal quotes("")
#' @param Flags list of possible varying parameters that are expanded to all possible combinations by expand.grid
#' @param AltName string alternate name for "Fun"
#' @param Prefix boolan default is FALSE.  TRUE would place the column meta before the column name.
#' @param FixedSep string divider of meta items
#' @param quote boolean passed to package DescTools function DoCall
#' @param envir calling environment
#' @param ... additional parameters
#' @return two dimensional object with different columns
#'
#' @references
#' \cite{Zachary Mayer, "Time series cross-validation 5"
#' \url{http://www.r-bloggers.com/time-series-cross-validation-5/}
#' \url{http://moderntoolmaking.blogspot.com/2013/01/time-series-cross-validation-5.html}
#' }
#'
#' @examples
#' \dontrun{
#'
#' library(quantmod)
#' ibm <- getSymbols("IBM", from = "1970-01-01", to = "1970-01-13", auto.assign = FALSE)
#'
#'   explode(ibm[,c("IBM.Open")], Fun = "TTR::SMA", Flags = list(n = 2:3))
#'
#' #           IBM.Open.TTR.SMA.n.2  IBM.Open.TTR.SMA.n.3
#' # 1970-01-02                   NA                   NA
#' # 1970-01-05               18.262                   NA
#' # 1970-01-06               18.356               18.312
#' # 1970-01-07               18.419               18.379
#' # 1970-01-08               18.431               18.425
#' # 1970-01-09               18.456               18.446
#' # 1970-01-12               18.463               18.454
#'
#'   explode(ibm[,c("IBM.Open","IBM.Close")], Fun = "TTR::SMA", Flags = list(n = 2:3))
#' #
#' #            IBM.Open.TTR.SMA.n.2 IBM.Close.TTR.SMA.n.2 IBM.Open.TTR.SMA.n.3
#' # 1970-01-02                   NA                    NA                   NA
#' # 1970-01-05               18.262                18.325                   NA
#' # 1970-01-06               18.356                18.419               18.312
#' # 1970-01-07               18.419                18.431               18.379
#' # 1970-01-08               18.431                18.456               18.425
#' # 1970-01-09               18.456                18.463               18.446
#' # 1970-01-12               18.463                18.419               18.454
#'
#' # R CRAN Package TTR function runCor
#' # runCor : function (x, y, n = 10, use = "all.obs", sample = TRUE, cumulative = FALSE)
#' explode(ibm[,c("IBM.Open","IBM.Close")], ibm[,c("IBM.Low","IBM.High")],
#'         Fun = "TTR::runCor", Flags = list(n = 4:5, sample = c(TRUE,FALSE)))
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom rlist list.zip
#' @importFrom DescTools DoCall
#' @export
explode <- function(  x1 = NULL, x2 = NULL, Fun = NULL
                         , Flags   = NULL
                         , AltName   = NULL, Prefix = NULL, FixedSep  = NULL
                         , quote     = FALSE, envir = parent.frame(2)
                         , ...){
tryCatchLog::tryCatchLog({

  if(is.null(x1)) stop("x1 is required")
  if(is.null(x2)) x2 <- eval(parse(text = paste0(class(x1)[1], "()")))
  if(is.null(FixedSep)) FixedSep = "."

  # if do.call(rlist::list.zip, ) ever fails then
  # then purrr::transpose is an acceptable replacement
  do.call(rlist::list.zip,as.list(DescTools::DoCall(expand.grid, Flags))) -> FlagsCombinations

  if(!NCOL(FlagsCombinations)){ return(eval(parse(text = paste0(class(x1)[1], "()")))) }

  if(mode(Fun) == "function") {
    Fun = match.fun(Fun)
    isCharFun <- FALSE
  } else {
    isCharFun <- TRUE
  }

  x <- eval(parse(text = paste0(class(x1)[1], "()")))
  FunctionEnv <- environment()

  lapply(FlagsCombinations, function(FlagsCombo) {

    lapply(pairWise(x1, x2), function(ObjectColumnSet) {

      x11 <- ObjectColumnSet[[1]]
      x21 <- ObjectColumnSet[[2]]

      if(NVAR(x21)) { x21List <- list(x21) } else { x21List <- NULL }
      Temp <- DescTools::DoCall(Fun, args = c(list(), list(x11), x21List, FlagsCombo, list(...)), quote = quote, envir = envir)

      Temp <- newColName( Temp, Fun = Fun, isCharFun = isCharFun, x1 = x11, x2 = x21, FlagsCombo = FlagsCombo
                             , AltName = AltName, Prefix = Prefix, FixedSep = FixedSep)

      assign("x", merge(x, Temp), envir = FunctionEnv)

      invisible()

    }) -> Empty

  }) -> Empty

  x

})}