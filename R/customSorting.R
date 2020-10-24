

#' custom sorting a vector
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
#' @export
cSort <- function(x, ...) UseMethod("cSort")



#' custom sorting a vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @export
cSort.default <- function(x, ...) stop("No cSort S3 method found")



#' custom sorting a vector
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
#'
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

#' @rdname cSort
#' @examples
#' \dontrun{
#'
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
#' @export
cSort.character <- function(x, InitOrder,
                            sortVectExc = T, sortVectExcCI = F,
                            chopVectExc = F, ...) {

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

}




#' custom sorting a vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @examples
#' \dontrun{
#'
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
#' @export
cSort.numeric <- function(x, InitOrder, ...) {

  x <- as.character(x)
  InitOrder <- as.character(InitOrder)
  x <- cSort(x, InitOrder = InitOrder, ...)
  as.numeric(x, ...)
}



#' custom sorting a vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @examples
#' \dontrun{
#'
#' # logical example
#'
#' cSort(c(F ,T, F, T, T, F, T, F), F)
#' [1] FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
#' }
#' @importFrom chron as.times
#' @export
cSort.logical <- function(x, InitOrder, ...) {

  x <- as.integer(x)
  InitOrder <- as.integer(InitOrder)
  x <- cSort(x, InitOrder = InitOrder, ...)
  as.logical(x)
}


#' custom sorting a vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @export
cSort.POSIXct <- function(x, InitOrder, ...) {

  x <- as.numeric(x)
  InitOrder <- as.numeric(InitOrder)
  x <- cSort(x, InitOrder = InitOrder, ...)
  as.POSIXct(x, origin = "1970-01-01", ...)

}


#' custom sorting a vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @export
cSort.POSIXlt <- function(x, InitOrder, ...) {

  x <- as.numeric(x)
  InitOrder <- as.numeric(InitOrder)
  x <- cSort(x, InitOrder = InitOrder, ...)
  as.POSIXlt(x, origin = "1970-01-01", ...)

}




#' custom sorting a vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @examples
#' \dontrun{
#'
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
#' @importFrom zoo as.Date
#' @export
cSort.Date <- function(x, InitOrder, ...) {

  x <- as.numeric(x)
  InitOrder <- as.numeric(InitOrder)
  x <- cSort(x, InitOrder = InitOrder, ...)
  zoo::as.Date(x, ...)
}



#' custom sorting a vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @importFrom zoo yearmon
#' @export
cSort.yearmon <- function(x, InitOrder, ...) {

  x <- as.numeric(x)
  InitOrder <- as.numeric(InitOrder)
  x <- cSort(x, InitOrder = InitOrder, ...)
  zoo::as.yearmon(x, ...)
}



#' custom sorting a vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @importFrom zoo yearqtr
#' @export
cSort.yearqtr <- function(x, InitOrder, ...) {

  x <- as.numeric(x)
  InitOrder <- as.numeric(InitOrder)
  x <- cSort(x, InitOrder = InitOrder, ...)
  zoo::as.yearqtr(x, ...)
}







#' custom sorting a vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @importFrom chron as.chron
#' @export
cSort.chron <- function(x, InitOrder, ...) {

  x <- as.POSIXct(x)
  InitOrder <- as.POSIXct(InitOrder)
  x <- cSort(x, InitOrder = InitOrder, ...)
  chron::as.chron(x)
}


#' custom sorting a vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @importFrom chron as.times
#' @export
cSort.times <- function(x, InitOrder, ...) {

  x <- as.character(x)
  InitOrder <- as.character(InitOrder)
  x <- cSort(x, InitOrder = InitOrder, ...)
  chron::as.times(x)
}



#' custom sorting a vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @importFrom chron as.chron as.dates
#' @export
cSort.dates <- function(x, InitOrder, ...) {

  # help from namespace xts
  # xts:::as.POSIXct.dates
  # need to implement our own method to correctly handle TZ
  x         <- structure(as.POSIXct(as.POSIXlt(x,         tz="GMT"), tz="GMT"),class=c("POSIXct","POSIXt"))
  InitOrder <- structure(as.POSIXct(as.POSIXlt(InitOrder, tz="GMT"), tz="GMT"),class=c("POSIXct","POSIXt"))
  x <- cSort(x, InitOrder = InitOrder, ...)
  chron::as.dates(as.chron(x))
}



#' custom sorting a vector
#'
#' @description
#' \preformatted{
#' }
#' @rdname cSort
#' @importFrom timeDate as.timeDate
#' @export
cSort.timeDate <- function(x, InitOrder, ...) {

  x <- as.POSIXct(x)
  xA <- attributes(x)
  InitOrder <- as.POSIXct(InitOrder)
  x <- cSort(x, InitOrder = InitOrder, ...)
  as.timeDate(x, zone = xA$tzone, FinCenter = xA$control[names(xA$control) %in% "FinCenter"])
}


