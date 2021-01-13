


# idea
#
# multi sort
# ---------
#
# Reduce
#   Sort -> ORDER\cSort  -> upon (if rle > 1), split -> Sort



#' Custom Sorting a Vector
#'
#' @description
#' \preformatted{
#' Given a finite known number of exact words, perform a
#' custom sort of the words in vector x.  The parameter InitOrder
#' contains the exact literal words to be sorted and in the exact order.
#'
#' This function is inspired by the custom sorting of database columns
#' that are done from inside the function dbWriteTable2
#' that is found inside the R CRAN package caroline.
#'
#' R language S3 class implementations exist for some classes from packages
#' and R CRAN packages: base, zoo, chron, and timeDate.
#' Vector words are members of the following classes:
#' character, numeric(integer), logical,
#' POSIXct, POSIXlt, Date, yearmon, yearqtr, chron, times, dates, timeDate
#'
#' Contributions are welcome.
#' }
#' @rdname cSort
#' @importFrom tryCatchLog tryCatchLog
#' @export
cSort <- function(x, ...) {
  UseMethod("cSort")
}



#' Custom Sorting a Vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @importFrom tryCatchLog tryCatchLog
#' @export
cSort.default <- function(x, ...) {
  tryCatchLog::tryCatchLog({
  stop("No cSort S3 method found")
  }, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Custom Sorting a Vector
#'
#' @description
#' \preformatted{
#' About excess vector elements(not found in InitOrder) that exist in 'x'
#' They may be appended to the end. chopVectExc = F (default)
#' Choices are sort or not-sort. sortVectExc = T(default)
#' Choices are CI(case insensitive) sortVectExcCI = T sort (default)
#' or case sensitive sortVectExcCI = F sort
#'
#' Excess Vector elements(in InitOrder)
#' that do not exist in 'x' are simply ignored.
#' }
#' @param x vector to be sorted
#' @param InitOrder starting custom sorting ( without the excess )
#' @param sortVectExc sort vector excess. T(default) whether or not Vector excess words
#' are attempted to be sorted (T) or not attempted to be sorted (F)
#' @param sortVectExcCI F(default) whether or not Vector excess words that are
#' not found in InitOrder are sorted 'not case insensitive'(T) or
#' 'not case sensitive'(F).  Sorting is done by "lower()"
#' @param chopVectExc chop vector excess. F(default) whether or not excess (x) elements
#' not found in InitOrder are removed
#' @param ... dots passed
#' @return vector sorted by InitOrder
#' @references
#' \cite{Custom Sorting in R \url{https://stackoverflow.com/questions/23995285/custom-sorting-in-r}}
#' @references
#' \cite{Case insensitive sort of vector of string in R \url{https://stackoverflow.com/questions/29890303/case-insensitive-sort-of-vector-of-string-in-r}}
#' @references
#' \cite{David Schruth, (2013). caroline: A Collection of Database, Data Structure, Visualization, and Utility Functions for R. R package version 0.7.6.
#' \url{https://CRAN.R-project.org/package=caroline}
#'}
#' @examples
#' \dontrun{
#' # word examples
#'
#' # Know exact words: "date", "o", "h", "l", "c", "v", "a"
#' # that may mean: Date, Open, High, Low, Close, Volume, Average
#' # are "custom sorted".
#' # Excess words placed at the end
#' # and the excess words, "E2" and "e3", are
#' # sorted in case-insensitive alphabetical order.
#' #
#' cSort( c("a","v", "E2", "c","l", "e3" ,"h","o","date"),
#'    InitOrder = c("date", "o", "h", "l", "c", "v", "a"), sortVectExcCI = T
#' )
#' [1] "date" "o"    "h"    "l"    "c"    "v"    "a"    "E2"   "e3"
#'
# # same as above (but excess words are removed)
# cSort( c("a","v", "E2", "c","l", "e3" ,"h","o","date"),
#    InitOrder = c("date", "o", "h", "l", "c", "v", "a"), chopVectExc = T
# )
# [1] "date" "o"    "h"    "l"    "c"    "v"    "a
#'
#' # excess, "B" and "b", is not sorted
#' cSort(c("B","b","A","a"), c("a","A"), sortVectExc = F)
#' [1] "a" "A" "B" "b"
#'
#' # excess, "B" and "b", are sorted case senstive (default)
#' cSort(c("B","b","A","a"), c("a","A"))
#' [1] [1] "a" "A" "b" "B"
#' }
#' @rdname cSort
#' @importFrom tryCatchLog tryCatchLog
#' @export
cSort.character <- function(x, InitOrder,
                            sortVectExc = T, sortVectExcCI = F,
                            chopVectExc = F, ...) {
tryCatchLog::tryCatchLog({
  Vector <- x
  # will reduce to vector
  # as.vector that strips the incoming vector down to an R base type
  Vector <- as.vector(Vector)
  InitOrder <- as.vector(InitOrder)
  # custom sorting
  VectorLevels <- InitOrder
  # will reduce to vector
  # note: R package base function setdiff
  #       executes as.vecto
  VectorExcess <- setdiff(Vector, VectorLevels)
  # sortVectExcCI only makes sense (at this current time) about characters.
  # Unless anyone ever creates an S3 class
  # and implements a tolower method (e.g. Roman Numerals?)
  if(sortVectExcCI == F && is.character(Vector) || !is.character(Vector)) {
    if(sortVectExc) {
      VectorExcessCS <-   sort(VectorExcess)
    } else {
      VectorExcessCS <-        VectorExcess
    }
    VectorExcessCaseDetermined <- VectorExcessCS
  } else {
    if(sortVectExc) {                       # forces to.character - S3?
      VectorExcessCI <-   VectorExcess[order(tolower(VectorExcess))]
    } else {
      VectorExcessCI <-   VectorExcess
    }
    VectorExcessCaseDetermined <- VectorExcessCI
  }
  VectorExcessCaseDeterminedLevels <- c(VectorLevels, VectorExcessCaseDetermined)
  VectorFactor <- factor(Vector, levels = VectorExcessCaseDeterminedLevels)
  Vector <- Vector[order(VectorFactor)]
  Vector <- as.vector(Vector)
  if(chopVectExc)
    Vector <- Vector[!Vector %in% VectorExcessCaseDetermined]
  Vector
} , write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Custom Sorting a Vector
#'
#' @description
#' \preformatted{
#' }
#' @examples
#' \dontrun{
#' # numeric examples
#'
#' cSort(c(5, 2, 3, 4 ,1 ), c(4, 2, 3))
#' [1] 4 2 3 1 5
#'
#' cSort(c(5, 2, 3.0001, 4 ,1 ), c(4, 2, 3.0001))
#' [1] 4.0000 2.0000 3.0001 1.0000 5.0000
#' class(.Last.value)
#' [1] "numeric"
#'
#' # integer(numeric) example
#'
#' cSort(c(5L, 2L, 3L, 4L ,1L ), c(4L, 2L, 3L))
#' [1] 4 2 3 1 5
#' class(.Last.value)
#' [1] "integer"
#' }
#' @rdname cSort
#' @importFrom tryCatchLog tryCatchLog
#' @export
cSort.numeric <- function(x, InitOrder, ...) {
tryCatchLog::tryCatchLog({
  x <- as.character(x)
  InitOrder <- as.character(InitOrder)
  x <- cSort(x, InitOrder = InitOrder, ...)
  as.numeric(x, ...)
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Custom Sorting a Vector
#'
#' @description
#' \preformatted{
#' }
#' @examples
#' \dontrun{
#' # logical example
#'
#' cSort(c(F ,T, F, T, T, F, T, F), F)
#' [1] FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
#' }
#' @rdname cSort
#' @importFrom chron as.times
#' @importFrom tryCatchLog tryCatchLog
#' @export
cSort.logical <- function(x, InitOrder, ...) {
tryCatchLog::tryCatchLog({
  x <- as.integer(x)
  InitOrder <- as.integer(InitOrder)
  x <- cSort(x, InitOrder = InitOrder, ...)
  as.logical(x)
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


#' Custom Sorting a Vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @importFrom tryCatchLog tryCatchLog
#' @export
cSort.POSIXct <- function(x, InitOrder, ...) {
tryCatchLog::tryCatchLog({
  x <- as.numeric(x)
  InitOrder <- as.numeric(InitOrder)
  x <- cSort(x, InitOrder = InitOrder, ...)
  as.POSIXct(x, origin = "1970-01-01", ...)
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


#' Custom Sorting a Vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @importFrom tryCatchLog tryCatchLog
#' @export
cSort.POSIXlt <- function(x, InitOrder, ...) {
tryCatchLog::tryCatchLog({
  x <- as.numeric(x)
  InitOrder <- as.numeric(InitOrder)
  x <- cSort(x, InitOrder = InitOrder, ...)
  as.POSIXlt(x, origin = "1970-01-01", ...)
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}




#' Custom Sorting a Vector
#'
#' @description
#' \preformatted{
#' }
#' @examples
#' \dontrun{
#' # Date examples
#'
#' cSort(zoo::as.Date(6:3), zoo::as.Date(1:4))
#' [1] "1970-01-04" "1970-01-05" "1970-01-06" "1970-01-07"
#'
#' cSort(zoo::as.Date(6:3), zoo::as.Date(1:4), sortVectExc = F)
#' [1] "1970-01-04" "1970-01-05" "1970-01-07" "1970-01-06"
#'
#' cSort(zoo::as.Date(6:3), zoo::as.Date(1:4), sortVectExc = F, chopVectExc = T)
#' [1] "1970-01-04" "1970-01-05"
#' }
#' @rdname cSort
#' @importFrom zoo as.Date
#' @importFrom tryCatchLog tryCatchLog
#' @export
cSort.Date <- function(x, InitOrder, ...) {
tryCatchLog::tryCatchLog({
  x <- as.numeric(x)
  InitOrder <- as.numeric(InitOrder)
  x <- cSort(x, InitOrder = InitOrder, ...)
  zoo::as.Date(x, ...)
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Custom Sorting a Vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo yearmon
#' @export
cSort.yearmon <- function(x, InitOrder, ...) {
tryCatchLog::tryCatchLog({
  x <- as.numeric(x)
  InitOrder <- as.numeric(InitOrder)
  x <- cSort(x, InitOrder = InitOrder, ...)
  zoo::as.yearmon(x, ...)
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Custom Sorting a Vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @importFrom tryCatchLog tryCatchLog
#' @export
cSort.yearqtr <- function(x, InitOrder, ...) {
tryCatchLog::tryCatchLog({
  x <- as.numeric(x)
  InitOrder <- as.numeric(InitOrder)
  x <- cSort(x, InitOrder = InitOrder, ...)
  zoo::as.yearqtr(x, ...)
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Custom Sorting a Vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom chron as.chron
#' @export
cSort.chron <- function(x, InitOrder, ...) {
tryCatchLog::tryCatchLog({
  x <- as.POSIXct(x)
  InitOrder <- as.POSIXct(InitOrder)
  x <- cSort(x, InitOrder = InitOrder, ...)
  chron::as.chron(x)
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


#' Custom Sorting a Vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom chron as.times
#' @export
cSort.times <- function(x, InitOrder, ...) {
tryCatchLog::tryCatchLog({
  x <- as.character(x)
  InitOrder <- as.character(InitOrder)
  x <- cSort(x, InitOrder = InitOrder, ...)
  chron::as.times(x)
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Custom Sorting a Vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @importFrom tryCatchLog tryCatchLog
#' @export
cSort.dates <- function(x, InitOrder, ...) {
tryCatchLog::tryCatchLog({
  # help from namespace xts
  # xts:::as.POSIXct.dates
  # need to implement our own method to correctly handle TZ
  x         <- structure(as.POSIXct(as.POSIXlt(x,         tz="GMT"), tz="GMT"),class=c("POSIXct","POSIXt"))
  InitOrder <- structure(as.POSIXct(as.POSIXlt(InitOrder, tz="GMT"), tz="GMT"),class=c("POSIXct","POSIXt"))
  x <- cSort(x, InitOrder = InitOrder, ...)
  chron::as.dates(as.chron(x))
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Custom Sorting a Vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom timeDate as.timeDate
#' @export
cSort.timeDate <- function(x, InitOrder, ...) {
tryCatchLog::tryCatchLog({

  # if not done elsewhere
  #correct for TZ
  oldtz <- Sys.getenv("TZ")
  if(oldtz!="GMT") {
    Sys.setenv(TZ="UTC")
  }

  x <- as.POSIXct(x)
  xA <- attributes(x)
  InitOrder <- as.POSIXct(InitOrder)
  x <- cSort(x, InitOrder = InitOrder, ...)
  x <- as.timeDate(x, zone = xA$tzone, FinCenter = xA$control[names(xA$control) %in% "FinCenter"])

  Sys.setenv(TZ=oldtz)
  x
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Custom Inserting/Appending Into R Objects
#'
#' This is a generalization of the R base package function "append".
#'
#' Insert/append elements. If the elements are part of a collection, then the length of all elements of the list must be the same.
#' Currently, (at this time), supported, are R objects: "list" and "data.frame".
#' "values" should be passed as a list. The new collection item names are taken from the List item names.
#'
#' The type conversion function "ValuesFunction" can (if supported by the S3 method), transform the data:
#' ValueFunction = "Rfunction(value)".
#'
#' @param x The R object, such that the values are to be inserted/appended to.
#' @param values List. The values to be included in the modified R object.
#' @param ... Dots passed.
#' @returns Modified R Object.
#' @rdname cAppend
#' @importFrom tryCatchLog tryCatchLog
#' @export
cAppend <- function(x, values, ...) {
  UseMethod("cAppend")
}



#' @rdname cAppend
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @export
cAppend.default <- function(x, values, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  IsListx <- is.list(x)

  if(!is.list(values)) {
    values <- list(values)
  }

  if("after" %in% Names(Dots)) {
    after <- Dots[["after"]]
  } else {
    after = length(x)
  }

  if("ReturnClass" %in% Names(Dots)) {
    ReturnClass <- Dots[["ReturnClass"]]
  }

  if("ValueFunction" %in% Names(Dots)) {
    ValueFunction <- Dots[["ValueFunction"]]
  }

  if(IsListx) {
    if(exists("ValueFunction", inherits = FALSE)) {
      Values <- lapply(values, function(value) {eval(parse(text=ValueFunction))})
    } else {
      Values <- values
    }
    # append(iris[1:2,], list(END = c("zz","yy"), BEGIN = c("aa","bb")), after = 3)
    x <- append(x, values = Values, after= after)
    if(exists("ReturnClass", inherits = FALSE)) {
      if(ReturnClass == "data.frame") {
        x <- data.frame(x)
      }
    }
  } else {
    stop("No cAppend S3 method found")
  }

  # catch-all (currently, this should never happen)
  if(!IsListx && is.list(x)){
    # return to non-list
    x <- DescTools::DoCall(c, c(list(), x))
  }

  return(x)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' @rdname cAppend
#' @importFrom tryCatchLog tryCatchLog
#' @examples
#' \dontrun{
#' cAppend(iris[1:2,],
#'   list(END = c("zz","yy"), BEGIN = c("aa","bb")),
#'   ValueFunction = "toupper(value)", after = 3
#'   )
#' cAppend(as.list(iris[1:2,]),
#'   list(END = c("zz","yy"), BEGIN = c("aa","bb")),
#'   ValueFunction = "toupper(value)", after = 3
#'   )
#' }
#' @export
cAppend.data.frame <- function(x, values, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)
  if("ValueFunction" %in% Names(Dots)) {
    ValueFunction <- Dots[["ValueFunction"]]
  }

  if(exists("ValueFunction", inherits = FALSE)) {
    cAppend(as.list(x), values = values, ValueFunction = ValueFunction, ReturnClass = "data.frame", ...)
  } else {
    cAppend(as.list(x), values = values, ReturnClass = "data.frame", ...)
  }

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}
