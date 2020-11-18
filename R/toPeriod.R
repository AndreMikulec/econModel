


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

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


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

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



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
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Convert a Date time series to a series of another frequency
#'
#' fillInteriorBy data values are done by "last observation carried forward".
#' NOT IMPEMENTED YET
#'
#' @param x xts object
#' @param Period Period to convert to. Default is "months".
#' See ? seq.POSIXt: "secs", "mins", "hours", "days", "weeks", "months", "quarters" or "years"
#' This is the aggregation (summary)
#' @param PeriodEnd Integer. Default is NULL meaning use the expected period end.  If Period is "weeks", the default is 7L, and this value can be  (1 - Monday, 2 - Tuesday, ... 7 - Sunday).  See ? DescTools::Weekday
#' @param FunEach Function. Has one argument: x; that is the xts object of the period. Default is identity. Function to be applied per period.  The value at the, per period, "last" positions is the "end of period"(EOP).
#' @param FunAll Function. Default is econModel::NC. Has two argemetnss: x; that is the xts object of all periods; EOPIndex is the index of EOP index values. This function to be applied across all periods.
#' @param fillInterior Logical. Default is TRUE. Created sub-Period data points.  The default is "days". NOT IMPLEMENTED YET.
#' @param fillInteriorBy String. If fillInterior is TRUE, then default is "days". Put in sub-Period data. NOT IMPLEMENTED YET.
#' @param Calendar Default is "UnitedStates/GovernmentBond". Calendar to use.  See ?? RQuantLib::Calendars.  NOT IMPLEMENTED YET.
#' @param BusDayConv Integer.  Default is 0L.  See \url{https://www.quantlib.org/reference/group__datetime.html} See ? RQuantLib::Enum ? RQuantLib::adjust and parameter bcd(Business Day Convention). 0L means if the Day falls on a Holiday (Holiday includes weekends), then Following: the first business day after the given holiday becomes the (new) adjusted date.
#' @return modified xts object
#' @importFrom tryCatchLog tryCatchLog
#' @examples
#' \dontrun{
#' x <- xts(c(3,363), zoo::as.Date(c(3,363)))
#' toPeriod(x, Period = "weeks", PeriodEnd = 4L)
#' toPeriod(x, Period = "weeks", PeriodEnd = 3L)
#' toPeriod(x, Period = "weeks", PeriodEnd = 2L)
#' toPeriod(x, Period = "weeks", PeriodEnd = 1L)
#'}
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom RQuantLib adjust
#' @importFrom zoo index na.locf
#' @importFrom xts xts as.xts first last
#' @importFrom xts tclass `tclass<-` tformat `tformat<-` tzone `tzone<-` xtsAttributes `xtsAttributes<-`
#' @export
toPeriod <- function(x, Period="months", PeriodEnd = NULL,
                     FunEach = identity, FunAll = NC,
                     fillInterior = T, fillInteriorBy = "days",
                     Calendar = "UnitedStates/GovernmentBond",
                     BusDayConv = 0L
                     ) {
tryCatchLog::tryCatchLog({

  oldtz <- Sys.getenv("TZ")
  if(oldtz!="UTC") {
    Sys.setenv(TZ="UTC")
  }
  assign("oldtz", oldtz, envir = environment())

  if(Period == "weeks" && is.null(PeriodEnd)) PeriodEnd <- 7L # Sunday

  FunEach <- match.fun(FunEach)
  FunAll  <- match.fun(FunAll)

  Origtclass <- xts::tclass(x); Origtformat <- xts::tformat(x); Origtzone <- xts::tzone(x)
  OrigxtsAttributes <- xts::xtsAttributes(x)

  y <- xts::as.xts( x[,0], zoo::index(x))
  y <- xts::as.xts(     y, as.POSIXct(index(x)))

  # seq must start early (because Late sequences (31st) to not expand correctly)
  # put at start of Period
  DateTimes <- seq(as.POSIXct(cut(xts::first(zoo::index(y)), Period)), to = xts::last(zoo::index(y)),  by = Period)
  if(xts::last(DateTimes) < xts::last(index(y))) {
    # append one more generated observations
    DateTimes <- seq(as.POSIXct(cut(xts::first(zoo::index(y)), Period)), by = Period, length.out = length(DateTimes) + 1L)
  }

  # if an irregular start/end of period
  # case "weeks"
  if(Period == "weeks")  {
    # Weekday returns the week day of the input date. (1 - Monday, 2 - Tuesday, ... 7 - Sunday
    # Monday, PeriodStart == 1L (start of Week)
    # Sunday, PeriodEnd   == 7L (end of week)
    # Because index(y) is contained in the range of DateTimes,
    #   then tail(DateTimes) is the end of period
    #   then I want to go backwards toward(and land on) the correct
    #     end of period (defined by PeriodEnd).
    #       Therefore, Shift is a (-)negative number.
    #       See DescTools::Weekday(DateTimes)
    Shift <- (PeriodEnd - 7L) * 24L * 3600L
    if(xts::last(zoo::index(y)) < xts::last(DateTimes + Shift - 1) ) {
      # ok to shift backwards
      DateTimes <- DateTimes + Shift

    } else {
      # shift forward
      DateTimes <- DateTimes + PeriodEnd  * 24L * 3600L
    }
  }

  # keep later duplicates
  DateTimes <- DateTimes[!duplicated(DateTimes, fromLast = T)]
  # subtract off one small number to get EOP
  # smallest number without R rounding
  DateTimes <- DateTimes - 1/19884107.8518 # .Machine$double.xmin

  # beginning may not be needed (keep what is needed)
  DateTimes <-  DateTimes[!DateTimes < xts::first(index(y))]

  # new
  y <- xts::xts(, DateTimes)

  indexYunremoved <- zoo::index(y)
  # remove y index values if they already exist in index x
  zoo::index(y) <- zoo::index(y)[!zoo::index(y) %in% as.POSIXct(zoo::index(x))]
  # dangerously assume that they will be merged in order
  x <- merge(xts::xts(x, as.POSIXct(zoo::index(x))),y)
  # operations almost
  # just the index values
  UnShiftedPeriodRegions <- cut(index(x), breaks = indexYunremoved,  labels = FALSE)
  # except I want backwards (not forward), so . . .
  # shift to the right (and remove one excess element located at the "last" )
  # new "last" positions of each PerodRegion is the EOP
  PeriodRegions <-  c(NA_integer_, UnShiftedPeriodRegions)[seq_along(zoo::index(x))]
  # early head NA values become region zero(0L)
  PeriodRegions[is.na(PeriodRegions)] <- 0L
  PeriodsList <- split(zoo::index(x), f = PeriodRegions)
  # Ops Each
  # apply per period and at "last" position is the EOP
  PeriodsListUpdated <- lapply(PeriodsList, function(xx) {FunEach(x[xx])})
  # rbind.xts
  x <- DescTools::DoCall(rbind, c(list(), PeriodsListUpdated))
  # Ops All
  x <- FunAll(x, EOPIndex = indexYunremoved)
  #
  # only interested in keeping the end of period values
  # note this contradicts:  fillInterior and  fillInteriorBy (COME_BACK)
  x <- x[index(x) %in% indexYunremoved]

  xts::tclass(x) <- Origtclass
  xts::tformat(x) <- Origtformat
  xts::tzone(x) <- Origtzone
  xts::xtsAttributes(x) <- OrigxtsAttributes
  # if the tclass of x (e.g. Date) is more coarse than POSIXct
  # then keep the later duplicates (if any)
  zoo::index(x) <- zoo::index(x)[!duplicated(index(x), fromLast = T)]

  # Business Day Conventions adjustment
  # if the day moves, then the new time
  # would be the "same time" on the new(moved) date
  IndexDates <- zoo::as.Date(zoo::index(x))
  IndexTimes <- as.POSIXct(IndexDates)
  indexTimeDiffs <- IndexTimes - as.POSIXct(IndexDates)
  NewDates <-  RQuantLib::adjust(Calendar, dates = zoo::as.Date(zoo::index(x)), bdc = BusDayConv)
  NewTimes <- as.POSIXct(NewDates) + indexTimeDiffs
  index(x) <- NewTimes

  xts::tclass(x) <- Origtclass; xts::tformat(x) <- Origtformat; xts::tzone(x) <- Origtzone
  xts::xtsAttributes(x) <- OrigxtsAttributes
  # if the tclass of x (e.g. Date) is more coarse than Origtclass[1]
  # then keep the later duplicates (if any)
  zoo::index(x) <- zoo::index(x)[!duplicated(index(x), fromLast = T)]

  colnames(x)[1] <- "V1"

  Sys.setenv(TZ=oldtz)

  x

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}




