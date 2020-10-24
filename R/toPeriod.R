


#' zoo seq.yearmon
#'
#' @description
#' \preformatted{
#' Originally meant to be a zoo S3 object.
#' Based on the code of seq.Date.
#'
#' The situation of "no existing seq.yearmon" drove me nuts!
#' Therefore I programmed one.
#'
#' Currently, unlike seq.Date, this function
#' does not have a difftime implementation.
#'
#' Currently, unlike seq.Date, this function
#' does not try to convert to POSIX__.
#'
#' NOTE: this implementations compliments the gist by adding "by methods"
#' See. the examples.
#'
#' }
#' @references
#' \cite{R zoo S3 object seq.yearqtr seq.yearmon
#' \url{https://gist.github.com/AndreMikulec/aceb20a0b6c170027b035519ca7a3adb}}
#' @param from  See ? seq.Date
#' @param to See ? seq.Date
#' @param by See ? seq.Date. See examples (below).
#' @param length.out  See ? seq.Date
#' @param along.with  See ? seq.Date
#' @param ... See ? seq.Date
#' @return zoo as.yearmon class
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo as.yearmon
#' @examples
#' \dontrun{
#'
#' seq.yearmon(as.yearmon("2000-01"), to = as.yearmon("2000-07"))
#' # Error in seq.yearmon(as.yearmon("2000-01"), to = as.yearmon("2000-07"))  :
#' #  exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified
#' # That is the expected output!
#'
#' seq.yearmon(as.yearmon("2000-01"), to = as.yearmon("2000-07"), length.out = 2)
#' # [1] "Jan 2000" "Jul 2000"
#'
#' seq.yearmon(as.yearmon("2000-01"), to = as.yearmon("2000-07"), length.out = 3)
#' # [1] "Jan 2000" "Apr 2000" "Jul 2000"
#'
#' seq.yearmon(as.yearmon("2000-01"), to = as.yearmon("2000-07"), length.out = 4)
#' # [1] "Jan 2000" "Mar 2000" "May 2000" "Jul 2000"
#'
#' seq.yearmon(as.yearmon("2000-01"), to = as.yearmon("2000-07"), by = 1/12)
#' # [1] "Jan 2000" "Feb 2000" "Mar 2000" "Apr 2000" "May 2000" "Jun 2000" "Jul 2000"
#'
#' seq.yearmon(as.yearmon("2000-01"), to = as.yearmon("2000-07"), by = 2/12)
#' # [1] "Jan 2000" "Mar 2000" "May 2000" "Jul 2000"
#'
#' seq.yearmon(as.yearmon("2000-01"), to = as.yearmon("2000-07"), by = "2 yearmons")
#' # [1] "Jan 2000" "Mar 2000" "May 2000" "Jul 2000"
#'
#' seq.yearmon(as.yearmon("2000-01"), to = as.yearmon("2000-07"), by = "2 months")
#' # [1] "Jan 2000" "Mar 2000" "May 2000" "Jul 2000"
#'
#' seq.yearmon(as.yearmon("2000-07"), to = as.yearmon("2000-01"), by = -1/12)
#' # [1] "Jul 2000" "Jun 2000" "May 2000" "Apr 2000" "Mar 2000" "Feb 2000" "Jan 2000"
#'
#' seq.yearmon(as.yearmon("2000-07"), to = as.yearmon("2000-01"), by = "-1 yearmons")
#' # [1] "Jul 2000" "Jun 2000" "May 2000" "Apr 2000" "Mar 2000" "Feb 2000" "Jan 2000"
#'
#' }
#' @export
seq.yearmon <- function(from, to, by, length.out = NULL, along.with = NULL, ...) {
tryCatchLog::tryCatchLog({

  # R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
  # zoo_1.7-13
  # R version 3.5.3 (2019-03-11)
  # zoo_1.8-4

  # exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified
  # seq.Date - missing

  if (missing(from))
      stop("'from' must be specified")
  if (!inherits(from, "yearmon"))
      stop("'from' must be a \"yearmon\" object")
  if (length(zoo::as.yearmon(from)) != 1L)
      stop("'from' must be of length 1")
  if (!missing(to)) {
      if (!inherits(to, "yearmon"))
          stop("'to' must be a \"yearmon\" object")
      if (length(zoo::as.yearmon(to)) != 1L)
          stop("'to' must be of length 1")
  }
  if (!missing(along.with)) {
      length.out <- length(along.with)
  }
  else if (!is.null(length.out)) {
      if (length(length.out) != 1L)
          stop("'length.out' must be of length 1")
      length.out <- ceiling(length.out)
  }
  status <- c(!missing(to), !missing(by), !is.null(length.out))
  if (sum(status) != 2L)
      stop("exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified")

  # seq.Date - by management

  if (missing(by)) {
      from <- unclass(zoo::as.yearmon(from))
      to <- unclass(zoo::as.yearmon(to))
      res <- seq.int(from, to, length.out = length.out)
      return(structure(res, class = "yearmon"))
  }

  # see seq.Date
  if (length(by) != 1L)
      stop("'by' must be of length 1")
  valid <- 0L
  # # no implementation
  # if (inherits(by, "difftime")) {
  #     by <- switch(attr(by, "units"), secs = 1/86400, mins = 1/1440,
  #     hours = 1/24, days = 1, weeks = 7) * unclass(by)
  # }
  # else
  if (is.character(by)) {
      by2 <- strsplit(by, " ", fixed = TRUE)[[1L]]
      if (length(by2) > 2L || length(by2) < 1L)
          stop("invalid 'by' string")
      valid <- pmatch(by2[length(by2)], c("months", "yearmons"))
      if (is.na(valid))
          stop("invalid string for 'by'")
      if (valid <= 2L) { # always
          by <- c(1/12, 1/12)[valid]
      if (length(by2) == 2L)
          by <- by * as.integer(by2[1L])
      }
      else by <- if (length(by2) == 2L)
          as.integer(by2[1L])
      else 1
  }
  else if (!is.numeric(by))
      stop("invalid mode for 'by'")
  if (is.na(by))
      stop("'by' is NA")

  if (valid <= 2L) {
      from <- unclass(zoo::as.yearmon(from))
      if (!is.null(length.out))
          res <- seq.int(from, by = by, length.out = length.out)
      else {
          to0 <- unclass( zoo::as.yearmon(to))
          res <- seq.int(0, to0 - from, by) + from
      }
      res <- structure(res, class = "yearmon")
  }
  # do not try to convert to POSIX__
  # so skip

  if (!missing(to)) {
      to <- zoo::as.yearmon(to)
      res <- if (by > 0)
          res[res <= to]
      else res[res >= to]
  }
  res

})}


#' zoo seq.yearqtr
#'
#' @description
#' \preformatted{
#' Originally meant to be a zoo S3 object.
#' Based on the code of seq.Date.
#'
#' The situation of "no existing seq.yearqtr" drove me nuts!
#' Therefore I programmed one.
#' Currently, unlike seq.Date, this function
#' does not have a difftime implementation.
#'
#' Currently, unlike seq.Date, this function
#' does not try to convert to POSIX__.
#'
#' NOTE: this implementations compliments the gist by adding "by methods"
#' See. the examples.
#'
#' }
#' @references
#' \cite{R zoo S3 object seq.yearqtr seq.yearmon
#' \url{https://gist.github.com/AndreMikulec/aceb20a0b6c170027b035519ca7a3adb}}
#' @param from  See ? seq.Date
#' @param to See ? seq.Date
#' @param by See ? seq.Date. See examples (below).
#' @param length.out  See ? seq.Date
#' @param along.with  See ? seq.Date
#' @param ... See ? seq.Date
#' @return zoo as.yearqtr class
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo as.yearqtr
#' @examples
#' \dontrun{
#' seq.yearqtr(as.yearqtr("2000 Q1"), to = as.yearqtr("2002 Q1"))
#' # Error in seq.yearqtr(as.yearqtr("2000 Q1"), to = as.yearqtr("2002 Q1")) :
#' #  exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified
#' # That was the expected output!
#'
#' seq.yearqtr(as.yearqtr("2000 Q1"), to = as.yearqtr("2002 Q1"), length.out = 2)
#' # [1] "2000 Q1" "2002 Q1"
#'
#' seq.yearqtr(as.yearqtr("2000 Q1"), to = as.yearqtr("2002 Q1"), length.out = 3)
#' # [1] "2000 Q1" "2001 Q1" "2002 Q1"
#'
#' seq.yearqtr(as.yearqtr("2000 Q1"), to = as.yearqtr("2002 Q1"), length.out = 4)
#' # [1] "2000 Q1" "2000 Q4" "2001 Q2" "2002 Q1"
#'
#' seq.yearqtr(as.yearqtr("2000 Q1"), to = as.yearqtr("2002 Q1"), by = 1/4)
#' # [1] "2000 Q1" "2000 Q2" "2000 Q3" "2000 Q4" "2001 Q1" "2001 Q2" "2001 Q3"
#' # [8] "2001 Q4" "2002 Q1"
#'
#' seq.yearqtr(as.yearqtr("2000 Q1"), to = as.yearqtr("2002 Q1"), by = 2/4)
#' # [1] "2000 Q1" "2000 Q3" "2001 Q1" "2001 Q3" "2002 Q1"
#'
#' seq.yearqtr(as.yearqtr("2000 Q1"), to = as.yearqtr("2002 Q1"), by = "2 yearqtrs")
#' # [1] "2000 Q1" "2000 Q3" "2001 Q1" "2001 Q3" "2002 Q1"
#'
#' seq.yearqtr(as.yearqtr("2000 Q1"), to = as.yearqtr("2002 Q1"), by = "2 quarters")
#' # [1] "2000 Q1" "2000 Q3" "2001 Q1" "2001 Q3" "2002 Q1"
#'
#' seq.yearqtr(as.yearqtr("2002 Q1"), to = as.yearqtr("2000 Q1"), by = -1/4)
#' # [1] "2002 Q1" "2001 Q4" "2001 Q3" "2001 Q2" "2001 Q1" "2000 Q4" "2000 Q3"
#' # [8] "2000 Q2" "2000 Q1"
#'
#' seq.yearqtr(as.yearqtr("2002 Q1"), to = as.yearqtr("2000 Q1"), by = "-1 yearqtrs")
#' # [1] "2002 Q1" "2001 Q4" "2001 Q3" "2001 Q2" "2001 Q1" "2000 Q4" "2000 Q3"
#' # [8] "2000 Q2" "2000 Q1"
#'
#'}
#' @export
seq.yearqtr <- function(from, to, by, length.out = NULL, along.with = NULL, ...) {
tryCatchLog::tryCatchLog({

  # R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
  # zoo_1.7-13
  # R version 3.5.3 (2019-03-11)
  # zoo_1.8-4

  # exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified
  # seq.Date - missing

  if (missing(from))
      stop("'from' must be specified")
  if (!inherits(from, "yearqtr"))
      stop("'from' must be a \"yearqtr\" object")
  if (length(zoo::as.yearqtr(from)) != 1L)
      stop("'from' must be of length 1")
  if (!missing(to)) {
      if (!inherits(to, "yearqtr"))
          stop("'to' must be a \"yearqtr\" object")
      if (length(zoo::as.yearqtr(to)) != 1L)
          stop("'to' must be of length 1")
  }
  if (!missing(along.with)) {
      length.out <- length(along.with)
  }
  else if (!is.null(length.out)) {
      if (length(length.out) != 1L)
          stop("'length.out' must be of length 1")
      length.out <- ceiling(length.out)
  }
  status <- c(!missing(to), !missing(by), !is.null(length.out))
  if (sum(status) != 2L)
      stop("exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified")

  # seq.Date - by management

  if (missing(by)) {
      from <- unclass(zoo::as.yearqtr(from))
      to <- unclass(zoo::as.yearqtr(to))
      res <- seq.int(from, to, length.out = length.out)
      return(structure(res, class = "yearqtr"))
  }

  # see seq.Date
  if (length(by) != 1L)
      stop("'by' must be of length 1")
  valid <- 0L
  # # no implementation
  # if (inherits(by, "difftime")) {
  #     by <- switch(attr(by, "units"), secs = 1/86400, mins = 1/1440,
  #     hours = 1/24, days = 1, weeks = 7) * unclass(by)
  # }
  # else
  if (is.character(by)) {
      by2 <- strsplit(by, " ", fixed = TRUE)[[1L]]
      if (length(by2) > 2L || length(by2) < 1L)
          stop("invalid 'by' string")
      valid <- pmatch(by2[length(by2)], c("quarters", "yearqtrs"))
      if (is.na(valid))
          stop("invalid string for 'by'")
      if (valid <= 2L) { # always
          by <- c(1/4, 1/4)[valid]
          if (length(by2) == 2L)
              by <- by * as.integer(by2[1L])
      }
      else by <- if (length(by2) == 2L)
          as.integer(by2[1L])
      else 1
  }
  else if (!is.numeric(by))
      stop("invalid mode for 'by'")
  if (is.na(by))
      stop("'by' is NA")

  if (valid <= 2L) {
      from <- unclass(zoo::as.yearqtr(from))
      if (!is.null(length.out))
          res <- seq.int(from, by = by, length.out = length.out)
      else {
          to0 <- unclass(zoo::as.yearqtr(to))
          res <- seq.int(0, to0 - from, by) + from
      }
      res <- structure(res, class = "yearqtr")
  }
  # do not try to convert to POSIX__
  # so skip

  if (!missing(to)) {
      to <- zoo::as.yearqtr(to)
      res <- if (by > 0)
          res[res <= to]
      else res[res >= to]
  }
  res

})}



#' setdiff any dimension-less  object
#'
#' @description
#' \preformatted{
#'
#' Unlike base::setdiff does not do "as.vector".
#' Therefore vector classes are not lost.
#' ( e.g. Date and POSIXt).
#'
#' To compare, it uses (same as setdiff.default) match.
#'
#' }
#'
#' @param x vector
#' @param y vector of elements to subtract off from x
#' @return vector from x, elements of y have been subtracted from x
#' @export
setDiff <- function (x, y) {
tryCatchLog::tryCatchLog({

  if(!is.null(dim(x)) || !is.null(dim(y)))
    stop("setDiff needs dimension-less x and y")

  unique(if (length(x) || length(y))
      x[match(x, y, 0L) == 0L]
  else x)
})}



#' Convert time series data to an monthly OHLC series
#'
#' @description
#' \preformatted{
#'
#' This is a wrapper around xts to.period.
#' In the case of period="months"
#' Months between the first date and the last
#' date will have and entry filled in.
#' It will get the "firstof" or "lastof"
#' months between dates as the same in the call.
#'
#' x note, "firstof", "lastof" broken in indexAt
#' x should have generated _new_ dates (It did not.)
#' x the first time of the period, the last time of the period
#' x use (program in) zoo yearmon and yearqtr to generate these
#' x x <- xts(2:363, zoo::as.Date(1:362))
#'
#' # weirdness or bugs of xts::to.period
#' # indexAt "startof" and "endof" return the same dates ("endof" dates)
#' # if indexAt == "firstof" then the returned index class is "POSIXct" "POSIXt"
#' # if indexAt == "lastof" then the returned index class is "Date"
#'
#' # STILL HAS WEEK AND DAY DEBUGGING TO GO
#'
#' }
#'
#' @param x ? xts::to.monthly and the index(x) class must
#' have an S3 method for "seq(by = "months")".
#' Classes "Date" and POSIXt are provided  S3 methods in package "base".
#' @param period period to convert to. Default is "months".
#' See ? xts::to.period: "seconds", "minutes", "hours", "days", "weeks", "months", "quarters", and "years"
#' This is the aggregation (summary)
#' @param indexAt "firstof"(default). Different from xts::to.monthly.
#' See ? xts::to.period: 'yearmon', 'yearqtr', 'firstof', 'lastof', 'startof', or 'endof'.
#' This is the data item date time type. ERROR: startof and endof return the same dates (endof dates).
#' @param drop.time See ? xts::to.period
#' @param name See ? xts::to.period
#' @param return.class See ? quantmod::getSymbols.FRED: ts, zoo, xts, or timeSeries
#' @param fillMissingDates TRUE(default). If indexAt is one of "firstof"
#' or "lastof", then fill in missing month dates, if any.
#' Otherwise FALSE, just pass throught to xts::to.monthly.
#' @param fillMissingData FALSE(default).  Estimate value.  See timeSeries::na.omit .Meant for test data (not prediction data and not validation data).  If TRUE, then the parameter fillMissingDates become TRUE. NOT IMPLEMENTED YET.
#' @param Alignment  Does the x period start date match the calendar period start date? If TRUE, then Alignment is "regular." Default is "regular".  Otherwise, choose "irregular". NOT IMPLEMENTED YET.
#' @param ... dots See. ? xts::to.monthly
#' @return See. ? xts::to.monthly
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom xts to.monthly
#' @importFrom DescTools Weekday
#' @examples
#' \dontrun{
#' x <- xts(2:362, zoo::as.Date(2:362))
#' x <- xts(c(2,362), zoo::as.Date(c(2,362)))
#' x <- xts(c(2,362), as.POSIXct(zoo::as.Date(c(2,362))))
#' toPeriod(x, indexAt = "firstof")
#' toPeriod(x, indexAt = "lastof")
#' toPeriod(x, indexAt = "yearmon")
#' toPeriod(x, indexAt = "yearqtr")
#'}
#' @export
toPeriod <- function(x, period='months',indexAt='firstof',drop.time=TRUE, name = NULL,
                     return.class = 'xts',
                     fillMissingDates = T, fillMissingData = F, Alignment = 'regular', ...) {
tryCatchLog::tryCatchLog({

  oldtz <- Sys.getenv("TZ")
  if(oldtz!="UTC") {
    Sys.setenv(TZ="UTC")
  }
  #
  assign("oldtz", oldtz, envir = environment())

  # doc of R CRAN  package xts
  # The ability to create an index using any of the
  # supported timeBased classes
  # (POSIXct, Date, dates, chron, timeDate, yearmon, yearqtr)

  #  return.class: Presently this may be ts, zoo, xts, or timeSeries
  PeriodReturned <- quantmod___convert.time.series(x, return.class = "xts")

  #
  # I do not know the nature of the inbound x
  # but if it can be an xts index

  # It is possible with a call to as.xts to convert objects of class
  # timeSeries, ts, irts, fts, matrix, data.frame, and zoo
  # ? xts::xtsible
  # try( {index(??) <- index(??)}, silent = T)

  # objective 1: of periods that have datums, need begin/end of that datum month

  # note: needed
  #  S3 seq.yearmon, seq.yearqtr
  PeriodPerioded <- xts::to.period(PeriodReturned,period=period,indexAt=indexAt,drop.time=drop.time,name=name,OHLC = F)

  # continue onward
  Period <- PeriodPerioded

  # fixes

  # if(indexAt == "firstof" && period == "months" && class(index) == "Date") {
  #   index(Period) <- zoo::as.Date(as.yearmon(index(Period)), frac = 0)
  # }
  # if(indexAt  == "lastof"  && period == "months" && class(index) == "Date") {
  #   index(Period) <- zoo::as.Date(as.yearmon(index(Period)), frac = 1)
  # }
  # if(indexAt == "firstof" && period == "quarter" && class(index) == "Date") {
  #   index(Period) <- zoo::as.Date(as.yearmon(index(Period)), frac = 0)
  # }
  # if(indexAt  == "lastof"  && period == "quarter" && class(index) == "Date") {
  #   index(Period) <- zoo::as.Date(as.yearmon(index(Period)), frac = 1)
  # }

  # make gaps of NA filling out time ranges between "periods that have datums"

  if(fillMissingDates) {

    # now need the
    # first day of the first period and the first day of the last period
    # in making seq.Date work, a person must start from day 1 of the period

    # methods(seq)
    # [1] seq.Date    seq.default seq.POSIXt

    # seq.Date # by # "day", "week", "month", "quarter" or "year"
                     # S3 dispatch

    # # stats,zoo, xts, timeSeries
    #  methods(as.yearmon)
    #  [1] as.yearmon.character* as.yearmon.date*      as.yearmon.Date*
    #  [4] as.yearmon.dates*     as.yearmon.default    as.yearmon.factor*
    #  [7] as.yearmon.integer*   as.yearmon.jul*       as.yearmon.mondate*
    # [10] as.yearmon.numeric*   as.yearmon.POSIXt*    as.yearmon.ti*
    # [13] as.yearmon.timeDate*  as.yearmon.yearqtr*
    #
    #  methods(as.yearqtr)
    #  [1] as.yearqtr.character* as.yearqtr.date*      as.yearqtr.Date*
    #  [4] as.yearqtr.dates*     as.yearqtr.default    as.yearqtr.factor*
    #  [7] as.yearqtr.integer*   as.yearqtr.jul*       as.yearqtr.mondate*
    # [10] as.yearqtr.numeric*   as.yearqtr.POSIXt*    as.yearqtr.ti*
    # [13] as.yearqtr.timeDate*  as.yearqtr.yearqtr*

    # now need the
    # first day of the first period and the first day of the last period
    # in making seq.Date work, a person must start from day 1 of the period

    if(period == "quarters") {
      Intermediates <- seq(from = utils::head(zoo::as.Date(as.yearqtr(index(Period)), frac = 0),1),
                           to   = utils::tail(zoo::as.Date(as.yearqtr(index(Period)), frac = 0),1), by = "quarter")
    } else
    if(period == "months") {
      Intermediates <- seq(from = utils::head(zoo::as.Date(as.yearmon(index(Period)), frac = 0),1),
                           to   = utils::tail(zoo::as.Date(as.yearmon(index(Period)), frac = 0),1), by = "month")
    } else
    if(period == "weeks") {
      Intermediates <- seq(from = utils::head(zoo::as.Date(index(Period)),1),
                           to   = utils::tail(zoo::as.Date(index(Period)),1), by = "day")
      # only Sundays # (not 'by = "week"' so I do not have to first figure out where Sunday exists.)
      Intermediates <- Intermediates[DescTools::Weekday(Intermediates) == 7L] # 1(Monday) - 7(Sunday)
    } else
    if(period == "days") {
      Intermediates <- seq(from = utils::head(zoo::as.Date(index(Period)),1),
                             to = utils::tail(zoo::as.Date(index(Period)),1), by = "day")
    }

    # # stats,zoo, xts, timeSeries
    # methods(as.POSIXct)
    # [1] as.POSIXct.Date      as.POSIXct.default
    # [3] as.POSIXct.IDate*    as.POSIXct.ITime*
    # [5] as.POSIXct.numeric   as.POSIXct.POSIXlt
    # [7] as.POSIXct.timeDate* as.POSIXct.yearmon*
    # [9] as.POSIXct.yearqtr*

    # and remove dupicates
    if(class(index(Period))[1] == "Date") {
      Intermediates <- unique(eval(parse(text = paste0("zoo::as.", "Date", "(Intermediates)"))))
    } else {
      Intermediates <- unique(eval(parse(text = paste0("as.", class(index(Period))[1], "(Intermediates)"))))
    }

    newDates <- setDiff(c(Intermediates, index(Period)), index(Period))
    Period <- merge(Period, xts(, newDates))

  }

  # remove duplicate dates (if not already done by setDiff in fillMissingDates)
  #
  # keep the earliest datum in the period
  if(indexAt %in% c("firstof", "startof")) {
     Period <- Period[!duplicated(index(Period)), ]
     if(period == "quarters")
       { Period <- Period[!duplicated(DescTools::Quarter(index(Period))), ] }
     if(period == "months")
       { Period <- Period[!duplicated(DescTools::Month(  index(Period))), ] }
     if(period == "weeks")
       { Period <- Period[!duplicated(DescTools::Week(   index(Period))), ] }
     if(period == "days")
       { Period <- Period[!duplicated(as.integer(zoo::as.Date(index(Period)))), ] }
  }
  # keep the latest datum in the period
  if(indexAt %in% c("endof", "lastof")) {
     Period <- Period[!duplicated(index(Period), fromLast = T), ]
     if(period == "quarters")
       { Period <- Period[!duplicated(DescTools::Quarter(index(Period)), fromLast = T), ] }
     if(period == "months")
       { Period <- Period[!duplicated(DescTools::Month(  index(Period)), fromLast = T), ] }
     if(period == "weeks")
       { Period <- Period[!duplicated(DescTools::Week(   index(Period)), fromLast = T), ] }
     if(period == "days")
       { Period <- Period[!duplicated(as.integer(zoo::as.Date(index(Period))), fromLast = T), ] }
  }

  # # stats,zoo, xts, timeSeries
  # methods(as.Date)
  # [1] as.Date.character as.Date.default   as.Date.factor    as.Date.mondate*
  # [5] as.Date.numeric   as.Date.POSIXct   as.Date.POSIXlt   as.Date.timeDate
  # [9] as.Date.ts        as.Date.yearmon   as.Date.yearqtr

  # convert the class of the index back to the original time series class
  if(class(index(PeriodPerioded))[1] == "Date") {
    index(Period) <- eval(parse(text = paste0("zoo::as.", "Date", "(index(Period))")))
  } else {
    index(Period) <- eval(parse(text = paste0("as.", class(index(PeriodPerioded))[1], "(index(Period))")))
  }

  Sys.setenv(TZ=oldtz)

  # return the desired class
  # I KNOW class(Period) is "xts"
  if(class(Period)[1] == "xts") {
    #  return.class: Presently this may be ts, zoo, xts, or timeSeries
    Period <- quantmod___convert.time.series(Period, return.class)
  }

  Period

})}

