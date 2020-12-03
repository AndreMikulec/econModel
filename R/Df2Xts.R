
#' Convert a Dataframe to an xts object or Vice Versa
#'
#' Of castDf2Xts, it is a wrapper over the R CRAN package zoocat function cast2zoocat
#' Convert a Dataframe to an xts object.
#'
#' @param  x Of castDf2Xts, a dataframe. Of meltXts2Df, an xts object. Of indname and `IndName<-` an xts object. Default is NULL. Required.
#' @param IndexVar String. Default is NULL. Required. The name of the column to be treated as the index of the xts object. Different from zoocat::cast2zoocat, this can be composed of two or three columns of the column names: c("year", "month") or c("year", "month", "day").
#' @param ValueVar String. Default is NULL. Required. The name of the column which stored the values.
#' @param AttrVar String.  Default is NULL. The name of the column which will be used as column attributes of the xts object. If NULL, all columns except ValueVar and IndexVar will be used.
#' @param FunAggr Function. Default is NULL. Aggregation function needed if variables do not identify a single observation for each output cell. Defaults to length (with a message) if needed but not specified. See ? reshape2::dcast.
#' @param DelUniqCAttr Logical. Default is TRUE. If TRUE, the column attributes with unique value will be deleted.
#' @param RetIndexClass String. Default is "POSIXct".  Attempt to return the xts with the index of this class.
#' @return Of castDf2Xts, an xts object.
#' @examples
#' \dontrun{
#' df <- data.frame(year = rep(1991 : 1995, each = 24), month = rep(1 : 12, 10),
#'                  varname = rep(c('a', 'b'), each = 12), city = rep(1 : 3, each = 40),
#'                  value = 1 : 120)
#'
#' castDf2Xts(df, IndexVar = "year", ValueVar = "value")
#'
#' # FunAggr = length
#' castDf2Xts(df, IndexVar = "year", ValueVar = "value",
#'   AttrVar = "varname")
#'
#' castDf2Xts(df, IndexVar = "year", ValueVar = "value",
#'   AttrVar = "varname", FunAggr = sum)
#'
#' castDf2Xts(df, IndexVar = c("year","month"), ValueVar = "value")
#'
#' castDf2Xts(df, IndexVar = "year", ValueVar = "value", RetIndexClass = "Date")
#'
#' casted <- castDf2Xts(df, IndexVar = c("year", "month"), ValueVar = "value",
#'   RetIndexClass = "Date")
#'
#' meltXts2Df(casted)
#' }
#' @rdname Df2Xts
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools StrPad
#' @importFrom zoo as.Date coredata
#' @importFrom xts xts `xtsAttributes<-`
#' @importFrom zoocat zoocat
#' @export
castDf2Xts <- function(x, IndexVar = NULL, ValueVar = NULL, AttrVar = NULL, FunAggr = NULL, DelUniqCAttr = F, RetIndexClass = "POSIXct") {
tryCatchLog::tryCatchLog({

  oldtz <- Sys.getenv("TZ")
  if(oldtz!="UTC") {
    Sys.setenv(TZ="UTC")
  }

  if(!is.null(FunAggr)) {
    FunAggr <- match.fun(FunAggr)
  }

  OrigColNames <- colnames(x)
  OrigIndexVar <- IndexVar
  OrigIndexClass <- class(IndexVar[1])

  # Composite IndexVar (variant)
  #
  if(1L < length(IndexVar)) {
    if(2L == length(IndexVar)) {
      if(tolower(IndexVar[1]) %in% c("year", "years") &&
         tolower(IndexVar[2]) %in% c("month", "months")
      )
      {
        x$yearmonth <- paste0(as.character(x[, IndexVar[1], drop = T]), "-", DescTools::StrPad(as.character(x[, IndexVar[2], drop = T]), width = 2, pad = "0", adj = "right"))
        x <- x[, !colnames(x) %in% c(IndexVar[1], IndexVar[2])  ,drop = F]
        IndexVar <- "yearmonth"
      }

    }
    if(3L == length(IndexVar)) {
      if(tolower(IndexVar[1]) %in% c("year", "years") &&
         tolower(IndexVar[2]) %in% c("month", "months") &&
         tolower(IndexVar[3]) %in% c("day", "days")
      )
      {
        x$yearmonthday <- paste0(as.character(x[, IndexVar[1], drop = T]), "-", DescTools::StrPad(as.character(x[, IndexVar[2], drop = T]), width = 2, pad = "0", adj = "right"), "-", DescTools::StrPad(as.character(x[, IndexVar[3], drop = T]), width = 2, pad = "0", adj = "right"))
        x <- x[, !colnames(x) %in% c(IndexVar[1], IndexVar[2], IndexVar[3])  ,drop = F]
        IndexVar <- "yearmonthday"
      }
    }
  }
  # column types must be basic R "vector types" (no Date and no POSIXct)
  x <- zoocat::cast2zoocat(x, index.var = IndexVar, value.var = ValueVar, attr.var = AttrVar, fun.aggregate = FunAggr, del.unique.cattr = DelUniqCAttr)

  # always fails
  r <- try(xts::xts(zoo::coredata(x), order.by = attr(x, "index")), silent = T)
  if(inherits(r, "try-error")) {

    Index <- index(x)
    # some intelligent guessing
    if(tolower(IndexVar) %in% c("years", "year")) {
      Index <- as.POSIXct(zoo::as.Date(paste0(as.character(Index),"-01-01")))
    }
    if(tolower(IndexVar) %in% c("yearmonth")) {
      Index <- as.POSIXct(zoo::as.Date(paste0(as.character(Index),"-01")))
    }
    if(tolower(IndexVar) %in% c("yearmonthday")) {
      Index <- as.POSIXct(zoo::as.Date(paste0(as.character(Index))))
    }
    if(RetIndexClass == "POSIXct") {
      Index <- as.POSIXct(as.numeric(Index), origin = "1970-01-01")
    } else {
      Index <- eval(parse(text = paste0("as.", RetIndexClass, "(Index)")))
    }
    x <- xts::xts(zoo::coredata(x), Index)
  } else {
    x <- r
  }

  if(is.null(AttrVar)) {
    xtsAttributes(x) <- list(fields = setdiff(setdiff(OrigColNames, OrigIndexVar), ValueVar))
  } else {
    xtsAttributes(x) <- list(fields = AttrVar)
  }

  if(tolower(IndexVar) %in% c("years", "year", "yearmonth", "yearmonthday")) {
    attr(x, "indname") <- "yearmonthday"
  } else {
    attr(x, "indname") <- OrigIndexVar
  }


  Sys.setenv(TZ=oldtz)
  x
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' x
#'
#' Of melt, convert an xts object to a data.frame. Expects an imported S3 "melt" function of the form of reshape2::melt.
#'
#' @param data Xts object. Default is NULL. Required. Xts object to melt.
#' @param value.name String. Default is NULL. The name of the column to store values.
#' @param index.name String. String. Default is the result of the function call indName(data).The name of the column used to store the index of the xts object.
#' @param na.rm Logical. Default is FALSE. As in R CRAN package reshape2 function melt. Should NA values be removed from the data set?
#' @param ... Dots Passed.
#' @return Of melt.xts, a dataframe.
#' @method melt xts
#' @rdname Df2Xts
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom reshape2 melt
#' @importFrom zoo coredata index
#' @importFrom xts xtsAttributes
#' @importFrom zoocat zoocat
melt.xts <- function (data, value.name = 'value', index.name = indName(data), na.rm = FALSE, ...) {
tryCatchLog::tryCatchLog({

  # CRAZY workaround index.name is always NULL
  if(is.null(index.name)) index.name = indName(data)

  # data
  # e.g.
  # [column attribute fields]: month, name
  # [index variable]: index # attr(x, "indname")
  #
  # create a "zoocat"
  #
  OrigColNames <- colnames(data)
  OrigColComponents <- strsplit(OrigColNames, "_")
  Fields <- as.list(as.data.frame(t(as.matrix(as.data.frame(OrigColComponents)))))
  names(Fields) <- xts::xtsAttributes(data)[["fields"]]
  data <- zoocat::zoocat(zoo::coredata(data), order.by = zoo::index(data), colattr = as.data.frame(Fields), index.name = index.name, ...)
  x <- melt(data, value.name = value.name, index.name = index.name, na.rm = na.rm, ...)
  x
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


#' x
#'
#' Of meltXts2Df, it is a wrapper over the R CRAN package zoocat function melt.
#' Convert an xts object to a dataframe.
#'
#' @param x Xts object. Default is NULL. Required. Xts object to melt.
#' @param ValueName String. Default is "value". The name of the column to store values..
#' @param IndexName String. Default is the result of the function call indName(x). The name of the column used to store the index of the xts object.
#' @param RmNA Logical. Default is FALSE. As in R CRAN package reshape2 function melt. Should NA values be removed from the data set?
#' @param ... Dots Passed.
#' @return Of meltXts2Df, a dataframe.
#' @rdname Df2Xts
#' @importFrom tryCatchLog tryCatchLog
#' @export
meltXts2Df <- function(x, ValueName = "value", IndexName = indName(data),  RmNA = F, ...) {
tryCatchLog::tryCatchLog({

  x <- melt(x, value.name = ValueName, index.name = IndexName, na.rm = RmNA, ...)
  x
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' x
#'
#' Of indName, it is a wrapper over the R CRAN package zoocat function indname.
#' Get the name of the index variable.
#'
#' @param x Xts object. Default is NULL. Required. The xts object.
#' @rdname Df2Xts
#' @return Of indName, String. Index variable name.
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoocat indname
#' @export
indName <- function(x) {
tryCatchLog::tryCatchLog({

  x <- attr(x, 'indname')
  x

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


#' x
#'
#' Of `indName<-`, is a wrapper over the R CRAN package zoocat function `indname<-`.
#' Set the name of the index variable.
#'
#' @param  x Of castDf2Xts, a dataframe. Of meltXts2Df, an xts object. Of indname and `IndName<-` an xts object. Default is NULL. Required.
#' @param value String. Default is NULL. Required. The new value.
#' @return Of `indName<-`, silently set index variable name.
#' @rdname Df2Xts
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoocat `indname<-`
#' @export
`indName<-` <- function(x, value) {
tryCatchLog::tryCatchLog({

  attr(x, "indname") <- value

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



