


#' Interleave Two Objects of Arbitrary Length
#'
#' @description
#' \preformatted{
#' interleave is from the (former) R CRAN package rmngb function interleave.
#' Instead, use multiInterleave.
#' }
#' @param x index-able object by `[` and the same index length as y.  Compatible type with y and combinable(c)
#' @param y index-able object by `[` and the same index length as x. Compatible type with x and combinable(c)
#' @return new combined object
#' @examples
#' \dontrun{
#' interleave(letters[1:2], LETTERS[1:2])
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
interleave <- function (x, y) {
tryCatchLog::tryCatchLog({

  if(sum(length(x), length(y)) %% 2L != 0L) {
    stop("length(x) + length(y) must be divisible by 2")
  }

  iX <- 2 * seq_along(x) - 1
  iY <- 2 * seq_along(y)
  c(x, y)[order(c(iX, iY))]

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


#' Interleave Multiple Objects of Arbitrary Length
#'
#' @description
#' \preformatted{
#' Inspired by the (former) R CRAN package rmngb function interleave.
#' }
#' @param ... Dots passed. Index-able objects by `[` and all objects have the same index length as each other.  All objects must have a compatible type with each other and combinable(c) with each other.
#' @return new combined object
#' @author Andre Mikulec
#' @examples
#' \dontrun{
#' multiInterleave(letters[1:2], LETTERS[1:2], 10:11)
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
multiInterleave <- function (...) {
tryCatchLog::tryCatchLog({

  List <- list(...)
  ListLen <- length(List)

  if(sum(unlist(lapply(List, length))) %% ListLen != 0L) {
    stop("The sum of all objects' elements must be divisible by the number of objects")
  }

  Order <-
    mapply(function(x, y) {
       ListLen * seq_along(x) - y
    }, List, as.list(rev(seq_len(ListLen) - 1L)))

  DescTools::DoCall(c, c(list(),List))[order(Order)]

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Pairwise Interleave of Two "Two Dimensional" Index-able Objects
#'
#' @description
#' \preformatted{
#' If one or the other has one index element while the other
#' has N index elements, then the one will be recycled to N index elements.
#' Consider, instead, using multiWise.
#' }
#' @param x two dimension index-able object
#' @param y two dimension index-able object
#' @return list of length two of two interleaved two dimensional index-able  objects
#' @examples
#' \dontrun{
#'
#' list(iris[1:2,1:2], airquality[1:2,1:2])
#' [[1]]
#'   Sepal.Length Sepal.Width
#' 1          5.1         3.5
#' 2          4.9         3.0
#'
#' [[2]]
#'   Ozone Solar.R
#' 1    41     190
#' 2    36     118
#'
#' str( pairWise( iris[1:2,1:2], airquality[1:2,1:2] ) )
#' List of 2
#'  $ :List of 2
#'   ..$ Sepal.Length: num [1:2] 5.1 4.9
#'   ..$ Ozone       : int [1:2] 41 36
#'  $ :List of 2
#'   ..$ Sepal.Width: num [1:2] 3.5 3
#'   ..$ Solar.R    : int [1:2] 190 118
#'
#' # 1 by N recycling
#' #
#'  str( pairWise( iris[1:2,1:2], airquality[1:2,1, drop = F] ) )
#'  List of 2
#'   $ :List of 2
#'    ..$ Sepal.Length: num [1:2] 5.1 4.9
#'    ..$ Ozone       : int [1:2] 41 36
#'   $ :List of 2
#'    ..$ Sepal.Width: num [1:2] 3.5 3
#'    ..$ Ozone      : int [1:2] 41 36
#'
#' library(xts)
#' data("sample_matrix", package = "xts")
#' str( pairWise(as.xts(sample_matrix)[,1:2], as.xts(sample_matrix)[,3:4] ) )
#' List of 2
#'  $ :List of 2
#'   ..$ Open:An 'xts' object on 2007-01-02/2007-06-30 containing:
#'   Data: num [1:180, 1] 50 50.2 50.4 50.4 50.2 ...
#'  - attr(*, "dimnames")=List of 2
#'   ..$ : NULL
#'   ..$ : chr "Open"
#'   Indexed by objects of class: [POSIXct,POSIXt] TZ:
#'   xts Attributes:
#'  NULL
#'   ..$ Low :An 'xts' object on 2007-01-02/2007-06-30 containing:
#'   Data: num [1:180, 1] 50 50.2 50.3 50.2 50.1 ...
#'  - attr(*, "dimnames")=List of 2
#'   ..$ : NULL
#'   ..$ : chr "Low"
#'   Indexed by objects of class: [POSIXct,POSIXt] TZ:
#'   xts Attributes:
#'  NULL
#'  $ :List of 2
#'   ..$ High :An 'xts' object on 2007-01-02/2007-06-30 containing:
#'   Data: num [1:180, 1] 50.1 50.4 50.4 50.4 50.2 ...
#'  - attr(*, "dimnames")=List of 2
#'   ..$ : NULL
#'   ..$ : chr "High"
#'   Indexed by objects of class: [POSIXct,POSIXt] TZ:
#'   xts Attributes:
#'  NULL
#'   ..$ Close:An 'xts' object on 2007-01-02/2007-06-30 containing:
#'   Data: num [1:180, 1] 50.1 50.4 50.3 50.3 50.2 ...
#'  - attr(*, "dimnames")=List of 2
#'   ..$ : NULL
#'   ..$ : chr "Close"
#'   Indexed by objects of class: [POSIXct,POSIXt] TZ:
#'   xts Attributes:
#'  NULL
#'
#' library(quantmod)
#' ibm <- quantmod::getSymbols("IBM", from = "1970-01-01", to = "1970-01-13", auto.assign = FALSE)
#' pairWise(tail(ibm[,c("IBM.Open","IBM.Close")]), xts(, zoo::as.Date(0)[0]))
#'
#' [[1]]
#' [[1]]$IBM.Open
#' IBM.Open
#' 1970-01-05   18.300
#' 1970-01-06   18.413
#' 1970-01-07   18.425
#' 1970-01-08   18.438
#' 1970-01-09   18.475
#' 1970-01-12   18.450
#'
#' [[1]][[2]]
#' Data:
#'   numeric(0)
#'
#' Index:
#'   integer(0)
#'
#' [[2]]
#' [[2]]$IBM.Close
#' IBM.Close
#' 1970-01-05    18.413
#' 1970-01-06    18.425
#' 1970-01-07    18.438
#' 1970-01-08    18.475
#' 1970-01-09    18.450
#' 1970-01-12    18.388
#'
#' [[2]][[2]]
#' Data:
#'   numeric(0)
#'
#' Index:
#'   integer(0)
#'
#' pairWise(tail(ibm[,c("IBM.Open")]), xts())
#' [[1]]
#' [[1]]$IBM.Open
#' IBM.Open
#' 1970-01-05   18.300
#' 1970-01-06   18.413
#' 1970-01-07   18.425
#' 1970-01-08   18.438
#' 1970-01-09   18.475
#' 1970-01-12   18.450
#'
#' [[1]][[2]]
#' Data:
#'   numeric(0)
#'
#' Index:
#'   integer(0)
#'
#'
#'}
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
pairWise <- function(x, y) {
tryCatchLog::tryCatchLog({

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

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Setwise Interleave of Many "Two Dimensional" Index-able Objects
#'
#' @description
#' \preformatted{
#' If any has less index elements than the others
#' then recycle the index elements.
#' }
#' @param ... Dots passed. Two dimension index-able objects Two or more collections of Index-able objects
#' @return
#' @examples
#' \dontrun{
#' multiWise(airquality[1:2,], iris[1:2,1:2], mtcars[1:2, 1:3])
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @export
multiWise <- function(...) {
  tryCatchLog::tryCatchLog({

    Dots <- list(...)

    # help from assertive.strings:::recycle

    # survey: longest set of elements
    LongestSet <- max(unlist(lapply(Dots, NVAR)))

    # recycle as necessary
    List <- lapply(Dots, function(x, length.out){
      L <- rep_len(as.list(x), LongestSet)
      if(!is.null(colnames(x))){
        names(L) <- rep_len(colnames(x), LongestSet)
      }
      L
    }, length.out = LongestSet)

    # unWeave
    Index <- seq_along(unlist(List, recursive = F)) %% length(Dots)
    UniqIndex <- unique(Index)
    List <- lapply(as.list(UniqIndex), function(x) {
      unlist(List, recursive = F)[Index == x]
    })
    List

  }, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' generate a good column name
#'
#' @description
#' \preformatted{
#'
#' Given column names of one or two two dimensional objects,
#' each with just one single column,  and a function and its arguments,
#' generate a new column name and return the original 'one column' two dimensional object
#'
#' }
#'
#' @param x object with the old column names
#' @param Fun literally written function or the function name inside a string.
#' if mode(Fun) == "function", then the returned (xts and) xts root column name is "Anon".
#' Alternately instead of "Anon", the use may (always) explicity provide the
#' return (xts and) xts root column name.
#' @param isCharFun hint as to Fun being a function or the function name inside of a string
#' @param x1 object providing part of the new column name
#' @param x2 object providing (yet another) part of the the new column name
#' @param FlagsCombo list with named elements and their values to (eventually) become part of the new column name
#' @param AltName column new root name.  Default is NULL. The input value is a string. Unless a string is provided then the root name will not be replaced.
#' @param asIsAltName Default is NULL(F). NULL is interpreted as F.
#' This NULL(F) means do not attempt to simplify the AltName.
#' For example, if the user passed "TTR::SMA" and asIsAltName == T, then "TTR::SMA" would
#' directly tried to become part of the column name (and "::" would cause an error so this
#' would not work.).  If the user passed "TTR::SMA" and asIsAltName == NULL(F), then
#' "SMA" would be directly tried to directly become part of the column name.
#' @param Prefix place the new addition to the column name at the front instead of the end. Default is NULL. Internally the default is FALSE.
#' @param FixedSep replacement for "[.]|::" that was found in the function name
#' @return object with the new column names
#' @export
#' @importFrom tryCatchLog tryCatchLog
newColName <- function(x = NULL, Fun =  NULL, isCharFun = NULL, x1 = NULL, x2 = NULL, FlagsCombo =  NULL,
                       AltName = NULL, asIsAltName = NULL, Prefix = NULL, FixedSep = NULL) {
tryCatchLog::tryCatchLog({

  if(is.null(isCharFun)) stop("newColNames need actual paramter isCharFun")

  if(is.null(AltName)) {
    if(isCharFun) {
      NewName <- tail(strsplit(Fun, "::")[[1]],1)
    } else {
      NewName <- "Anon"
    }
  } else { # AltName provided: it an only be a character string
    if(is.null(asIsAltName) || asIsAltName == F) {
      # try to fix
      NewName <- tail(strsplit(AltName, "::")[[1]],1)
    } else { # just leave the AltName(to become NewName) "as is"
      NewName <- AltName
    }

  }

  paste0(
    c(
      if(!is.null(x1) && (NCOL(x1) > 0)) { colnames(x1)[1] } else { NULL },
      if(!is.null(x2) && (NCOL(x2) > 0)) { colnames(x2)[1] } else { NULL }
    ), collapse = FixedSep) -> Colnames
    # consistency with the following
    if(Colnames == "") { Colnames <- NULL }

  # "TRUE" -> "T" "FALSE" -> "F"
  FlagsCombo <- lapply(FlagsCombo, prntPrpr)
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

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



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
#' @param asIsAltName Default is NULL(F). Passed to the column generator function.  NULL is interpreted as F.
#' This NULL(F) means do not attempt to simplify the AltName.
#' For example, if the user passed "TTR::SMA" and asIsAltName == T, then "TTR::SMA" would
#' directly tried to become part of the column name (and "::" would cause an error so this
#' would not work.).  If the user passed "TTR::SMA" and asIsAltName == NULL(F), then
#' "SMA" would be directly tried to directly become part of the column name.
#' @param Prefix boolan default is FALSE.  TRUE would place the column meta before the column name.
#' @param FixedSep string divider of meta items
#' @param quote boolean passed to package DescTools function DoCall
#' @param envir calling environment
#' @param ... additional parameters
#' @return two dimensional object with new and different columns
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
#' explode(ibm[,c("IBM.Open")], Fun = "TTR::SMA", Flags = list(n = 2:3))
#
#'           IBM.Open.TTR.SMA.n.2  IBM.Open.TTR.SMA.n.3
#' 1970-01-02                   NA                   NA
#' 1970-01-05               18.262                   NA
#' 1970-01-06               18.356               18.312
#' 1970-01-07               18.419               18.379
#' 1970-01-08               18.431               18.425
#' 1970-01-09               18.456               18.446
#' 1970-01-12               18.463               18.454
#'
#' explode(ibm[,c("IBM.Open","IBM.Close")], Fun = "TTR::SMA", Flags = list(n = 2:3))
#'
#'            IBM.Open.TTR.SMA.n.2 IBM.Close.TTR.SMA.n.2 IBM.Open.TTR.SMA.n.3
#' 1970-01-02                   NA                    NA                   NA
#' 1970-01-05               18.262                18.325                   NA
#' 1970-01-06               18.356                18.419               18.312
#' 1970-01-07               18.419                18.431               18.379
#' 1970-01-08               18.431                18.456               18.425
#' 1970-01-09               18.456                18.463               18.446
#' 1970-01-12               18.463                18.419               18.454
#'
#' # column naming checks
#'
#' explode(ibm[,c("IBM.Open","IBM.Close")], Fun = TTR::SMA, Flags = list(n = 2:3),
#'         AltName = "CUST")
#' IBM.Open.CUST.n.2 IBM.Close.CUST.n.2 IBM.Open.CUST.n.3 IBM.Close.CUST.n.3
#'
#' explode(IBM.Open.TTR.SMA.n.2, Fun = TTR::SMA, Flags = list(n = 3:4))
#' IBM.Open.TTR.SMA.n.2.TTR.SMA.n.3 IBM.Open.TTR.SMA.n.2.TTR.SMA.n.4
#'
#' SMA2 <- TTR::SMA
#' explode(IBM.Open.TTR.SMA.n.2, Fun = SMA2, Flags = list(n = 3:4))
#' IBM.Open.TTR.SMA.n.2.SMA2.n.3 IBM.Open.TTR.SMA.n.2.SMA2.n.4
#'
#' # x2 case
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
#' @importFrom xts xts
#' @importFrom xts tclass `tclass<-` tformat `tformat<-` tzone `tzone<-` xtsAttributes `xtsAttributes<-`
#' @export
explode <- function(  x1 = NULL, x2 = NULL, Fun = NULL
                         , Flags   = NULL
                         , AltName   = NULL, asIsAltName = NULL
                         , Prefix = NULL, FixedSep  = NULL
                         , quote     = FALSE, envir = parent.frame(2)
                         , ...){
tryCatchLog::tryCatchLog({

  if(is.null(x1)) stop("x1 is required")
  if(is.null(Fun)) stop("Fun is required")
  # possible alt: if is.null(x2) could run identity on x1

  if(is.null(x2)) x2 <- eval(parse(text = paste0(class(x1)[1], "()")))
  if(is.null(FixedSep)) FixedSep = "."

  # inside the call to newColName, function name of Fun is lost
  if(mode(Fun) == "function" && is.null(AltName)) {
    # e.g. [1] "::"  "TTR" "SMA"
    AltNameVector <- as.character(as.list(match.call())$Fun)
    if(AltNameVector[1] == "::" && length(AltNameVector) == 3)  {
      # rebuild the Altname
      # e.g. [1] "TTR::SMA"
      AltName  <- paste0(AltNameVector[2], AltNameVector[1], AltNameVector[3], collapse = "")
    } else {
      AltName <-  AltNameVector[1] # no change
    }

  }
  if(mode(Fun) == "function") {
    Fun <- match.fun(Fun)
    isCharFun <- FALSE
  } else {
    isCharFun <- TRUE
  }

  # if do.call(rlist::list.zip, ) ever fails then
  # then purrr::transpose is an acceptable replacement
  DescTools::DoCall(
    rlist::list.zip, as.list(DescTools::DoCall(expand.grid, c(list(), Flags, stringsAsFactors = F)))
  ) -> FlagsCombinations
  #
  # user just may want to run without any flag combinations
  if(!length(FlagsCombinations)){
    FlagsCombinations <- list(list())
  }

  # x: placeholder to return data into something
  x <- eval(parse(text = paste0(class(x1)[1], "()")))
  # these functions will not work (assign) to a zero width object
  #
  # xts::tclass(x)  <- xts::tclass(x1)
  # xts::tzone(x)   <- xts::tzone(x1)
  # xts::tformat(x) <- xts::tformat(x1)
  # xts::xtsAttributes(x) <- xts::xtsAttributes(x1)

  FunctionEnv <- environment()

  lapply(FlagsCombinations, function(FlagsCombo) {

    lapply(pairWise(x1, x2), function(ObjectColumnSet) {

      x11 <- ObjectColumnSet[[1]]
      x21 <- ObjectColumnSet[[2]]

      if(NVAR(x21)) { x21List <- list(x21) } else { x21List <- NULL }
      Temp <- DescTools::DoCall(Fun, args = c(list(), list(x11), x21List, FlagsCombo, list(...)), quote = quote, envir = envir)

      Temp <- newColName( Temp, Fun = Fun, isCharFun = isCharFun, x1 = x11, x2 = x21, FlagsCombo = FlagsCombo,
                                AltName = AltName, asIsAltName = asIsAltName,
                                Prefix = Prefix, FixedSep = FixedSep)

      # prevent loosing tclass, tzone, and tformat
      if(!NVAR(x)) {
        assign("x", Temp, envir = FunctionEnv)
      } else {
        assign("x", merge(x, Temp), envir = FunctionEnv)
      }

      invisible()

    }) -> Empty

  }) -> Empty

  # user side custom attributes
  # still there (nothing to do)
  # xts::xtsAttributes(x) <- xtsAttributes(x1)
  x

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}
