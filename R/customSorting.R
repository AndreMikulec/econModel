

#' custom sorting a vector
#'
#' @description
#' \preformatted{
#' Performs a custom sort of a vector x.  The parameter vector InitOrder,
#' contains the elements to be sorted and the exact order.
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
#' Excess Vector elements(not found in InitOrder) are appended
#' to the end. Choices are (sort or not sort, CI(case insensitive) sort
#' or non-CI sort)
#'
#' For the purpose of "not sorting",
#' other vector elements(vector excess) that are
#' in the parameter vector InitOrder that are
#' "not found" in the input x are ignored.
#' }
#'
#' @param x vector to be sorted
#' @param InitOrder starting custom sorting ( without the excess )
#' @param CI FALSE(default) whether or not Vector excess items that are
#' not found in InitOrder  are sorted 'not case insensitive'(TRUE) or
#' 'not case sensitive'(FALSE).  Sorting is done by "lower()"
#' @param sortVectExc sort vector excess. TRUE(default) whether or not Vector excess columns
#' are attempted to be sorted (TRUE) or not attempted to be sorted (FALSE)
#' @param chopVectExc chop vector excess. FALSE(default) whether or not excess (x) elements
#' not found in InitOrder are removed
#' @param ... dots passed
#' @return vector sorted by InitOrder
#' @references
#' \cite{Custom Sorting in R \url{https://stackoverflow.com/questions/23995285/custom-sorting-in-r}}
#' @references
#' \cite{Case insensitive sort of vector of string in R \url{https://stackoverflow.com/questions/29890303/case-insensitive-sort-of-vector-of-string-in-r}}
#' @rdname cSort
#' @examples
#' \dontrun{
#'
#' # character examples
#'
#' cSort( c("a","v", "E2", "c","l", "e3" ,"h","o","date"),
#'    InitOrder = c("date", "o", "h", "l", "c", "v", "a"), CI = TRUE
#' )
#' [1] "date" "o"    "h"    "l"    "c"    "v"    "a"    "E2"   "e3"
#'
#' cSort(c("E","B","C","D","A"), c("D","B","C"), sortVectExc = FALSE)
#' [1] "D" "B" "C" "E" "A"
#'
#' # excess(Vector)  "G", "F"
#' # cSort(c("G", "D","B","C", "F"), c("E","B","C","D","A"))
#' [1]"B" "C" "D" "F" "G"
#'
#' cSort(c("G", "D","B","C", "F"), c("E","B","C","D","A"), sortVectExc = FALSE)
#' [1] "B" "C" "D" "G" "F"
#'
#' # other(InitOrder) ignored "F"
#' cSort(c("E","B","C","D","A"), c("F", "D","B","C"), sortVectExc = FALSE)
#' [1] "D" "B" "C" "E" "A"
#' }
#' @export
cSort.character <- function(x, InitOrder, CI = FALSE, sortVectExc = TRUE, chopVectExc = FALSE, ...) {

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
  # CI only makes sense (at this current time) about characters.
  # Unless anyone ever creates an S3 class
  # and implements a tolower method (e.g. Roman Numerals?)
  if(CI == FALSE && is.character(Vector)) {
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


# So R Studio can see
cSort.numeric <- function() {}
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
#' cSort(c(5L, 2L, 3L, 4L ,1L ), c(4L, 2L, 3L))
#' class(.Last.value)
#' [1] "integer"
#' }
#' @export
# R studio can not debug this in "this way"
cSort.numeric <- cSort.character



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
#' cSort(zoo::as.Date(6:3), zoo::as.Date(1:4), sortVectExc = FALSE)
#' [1] "1970-01-04" "1970-01-05" "1970-01-07" "1970-01-06"
#'
#' cSort(zoo::as.Date(6:3), zoo::as.Date(1:4), sortVectExc = FALSE, chopVectExc = TRUE)
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
#' @importFrom chron as.chron
#' @export
cSort.chron <- function(x, InitOrder, ...) {

  x <- as.POSIXct(x)
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
  x <- structure(as.POSIXct(as.POSIXlt(x, tz="GMT"), tz="GMT"),class=c("POSIXct","POSIXt"))
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
  x <- cSort(x, InitOrder = InitOrder, ...)
  as.timeDate(x, zone = xA$tzone, FinCenter = xA$control[names(xA$control) %in% "FinCenter"])
}


