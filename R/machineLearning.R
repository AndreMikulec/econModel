
#' Load and Manage Data from Multiple Sources
#'
#' @description
#' \preformatted{
#'
#' This is a wrapper over the R CRAN package quantmod function getSymbols
#' with the additional paramter source.envir.
#'
#' Get some Symbols from an environment (source.envir).
#' It will search first in (source.envir).
#' If the Symbol is not found in the enviroment (source.envir),
#' then it gets the Symbol from elsewhere.
#'
#' NOTE: do not do a call with paramters: ("source.envir = e, "env = e")
#' when auto.assign = TRUE(default).
#' GetSymbolsEnv places Symbols into "env = e"
#' }
#'
#' @param Symbols	Character vector.  Required. Specifies the names of each symbol to be loaded.
#' @param env Environment.  Default is parent.frame(). Where to create objects. Setting env=NULL is equal to auto.assign=FALSE.
#' @param reload.Symbols Logical. Default is FALSE. To reload current symbols in specified environment.
#' @param verbose Logical. Default is FALSE. To turn on status of retrieval.
#' @param warnings Logical. Default is TRU. To turn on warnings.
#' @param src Character string. Default is "yahoo".  Specifying sourcing method.
#' @param symbol.lookup Logical. Default is TRUE. Retrieve symbol's sourcing method from external lookup.
#' @param auto.assign Logical. Default is getOption("getSymbols.auto.assign", TRUE). Should results be loaded to env. If FALSE, return results instead.
#' As of 0.4-0, this is the same as setting env=NULL. Defaults to TRUE.
#' @param source.envir Environment.  Default is NULL. If set to an Environment, will first(1) try to find the Symbol(s) in the Environment.  If the Symbol(s) are not found in the environment, then second(2) the Symbol(s) are (tried to be) acquired using the R CRAN package quantmod function getSymbols.
#' @param ... Dots passed to the sourcing method.
#' @return Called for its side-effect with env set to a valid environment and auto.assign=TRUE, getSymbols will load into the specified env one object for each Symbol specified, with class defined by return.class. Presently this may be ts, zoo, xts, or timeSeries.
#' If env=NULL or auto.assign=FALSE an object of type return.class will be returned.
#' @examples
#' \dontrun{
#' e <- new.env(parent = emptyenv())
#'
#' # Symbols are loaded into the environment "e".
#' quantmod::getSymbols(list(AAPL = "yahoo"), env = e)
#' ls.str(envir = e)
#'
#' # Instead of "yahoo", try first(1) get the Symbol(s) from the source.envir
#' # if not found in "e", then try to get the Symbol(s) from "yahoo"
#' AAPLSymbol <- getSymbolsEnv(list(AAPL = "yahoo"),
#'                  auto.assign = FALSE, source.envir = e)
#' str(AAPLSymbol)
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom quantmod getSymbols
#' @export
getSymbolsEnv <- function (Symbols = NULL, env = parent.frame(), reload.Symbols = FALSE,
          verbose = FALSE, warnings = TRUE, src = "yahoo", symbol.lookup = TRUE,
          auto.assign = getOption("getSymbols.auto.assign", TRUE),
          source.envir = NULL,
          ...) {
tryCatchLog::tryCatchLog({

  if(!is.null(source.envir)) {

    if (is.character(Symbols)) {
      current.symbols <- unlist(strsplit(Symbols, ";"))
    } else if(is.list(Symobls)) {
      current.symbols <- names(Symbols)
    }

    symbols.returned.from.envir <- character()
    for(current.symbols_i in current.symbols) {
      if(exists(current.symbols_i, envir = source.envir)) {
        symbols.returned_i <- list()
        symbols.returned_i[[current.symbols_i]] <- get(current.symbols_i, envir = source.envir)
        if (!auto.assign || is.null(env)) {
          symbols.returned.from.envir <- symbols.returned_i[[current.symbols_i]]
          # then can only EVER return ONE Symbol
          return(symbols.returned.from.envir)
        } else {
          assign(current.symbols_i, symbols.returned_i[[current.symbols_i]], env)
          symbols.returned.from.envir <- append(symbols.returned.from.envir, current.symbols_i)
        }
        # works
        current.symbols <- setdiff(current.symbols, current.symbols_i)
      }
    }
    # loop temp variable
    rm(current.symbols_i)

    # In a list with named elements,
    # the actual realized Symbols returned from the environment
    # symbols.returned.from.envir
    #
    # In a vector, the "remaining Symbols" not found in the environment
    # current.symbols
    #
    if(is.character(Symbols)) {
      Symbols <- paste0(current.symbols, collapse = ";")
    } else if(is.list(Symbols)) {
      Symbols <- Symbols[names(Symbols) %in% current.symbols]
    }
  }

  # if any
  returned <- quantmod::getSymbols(Symbols = Symbols, env = env, reload.Symbols = reload.Symbols,
                verbose = verbose, warnings = warnings, src = src, symbol.lookup = symbol.lookup,
                auto.assign = auto.assign,
                ...)

  if(!auto.assign || is.null(env)) {
    # then can only EVER return ONE Symbol
    return(returned)
  }

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Extract Dataset Created by specifyModel
#'
#' @description
#' \preformatted{
#' This is a re-implementation of the R CRAN package quantmod
#' function modelData. This includes the additional flexiblity that
#' data.window also can be a list of character vectors.
#'
#' Extract from a quantmod object the dataset
#' created for use in modeling.
#'
#' specifyModel creates a zoo object for use in subsequent
#' workflow stages (buildModel, tradeModel) that
#' combines all model inputs, from a variety of sources,
#' into one model frame.
#'
#' modelData returns this object.
#' }
#' @details
#' \preformatted{
#' When a model is created by specifyModel,
#' it is attached to the returned object.
#' One of the slots of this S4 class is model.data.
#' }
#' @param x	A quantmod object. Required.
#' @param data.window	Default is NULL.  A character vector of subset start and end dates to return. Alternately, this can be a list of character vectors.
#' @param exclude.training Logical. Default is FALSE. Remove the training period.
#' @return An object of class zoo containing all transformations to data specified in specifyModel.
#' @author Andre Mikulec (re-implementation)
#' @author Jeffrey A. Ryan
#' @export
modelData <- function (x, data.window = NULL, exclude.training = FALSE) {
tryCatchLog::tryCatchLog({
  model.data <- x@model.data

  if (!is.null(data.window)) {
    if(is.vector(data.window)) {
      if (length(data.window) > 2) {
        model.data <- model.data[index(model.data) %in% data.window]
      }
      else {
        start.date.index <- index(model.data[which(index(model.data) >=
                                                  as.Date(data.window[1], origin = "1970-01-01"))])
        end.date.index <- index(model.data[which(index(model.data) <=
                                                  as.Date(data.window[2], origin = "1970-01-01"))])
        date.range <- as.Date(intersect(start.date.index,
                                        end.date.index), origin = "1970-01-01")
        model.data <- model.data[date.range]
      }
    }
    # "new" additional flexibility
    if(list(data.window)) {
      model.data <- lapply(data.window, function(x) {
        if (length(data.window) > 2) {
          model.data <- model.data[index(model.data) %in% data.window]
        }
        else {
          start.date.index <- index(model.data[which(index(model.data) >=
                                                       as.Date(data.window[1], origin = "1970-01-01"))])
          end.date.index <- index(model.data[which(index(model.data) <=
                                                     as.Date(data.window[2], origin = "1970-01-01"))])
          date.range <- as.Date(intersect(start.date.index,
                                          end.date.index), origin = "1970-01-01")
          model.data <- model.data[date.range]
        }
        model.data
      })
    }
    # Dates with lapply and sapply
    # https://stackoverflow.com/questions/14449166/dates-with-lapply-and-sapply
    model.data <- DescTools::DoCall(c, model.data)
  }

  if (exclude.training == TRUE) {
    model.data <- model.data[!index(model.data) %in% x@training.data]
  }
  return(model.data)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' low-level set the values quantmod object slots
#'
#' @description
#' \preformatted{
#' Set the values quantmod object slots.
#' }
#' @examples
#' \dontrun{
#' data(sample_matrix, package = "xts")
#' modelData(specmodel) <- list(model.data = as.xts(sample.matrix))
#' }
#' @param x quantmod object
#' @param ... list of name-value pairs
#' @importFrom tryCatchLog tryCatchLog
#' @export
`modelData<-` <- function(x, ...) {
tryCatchLog::tryCatchLog({

  # maybe check for a valid slot name
  Dots <-list(...)$value
  for(i in names(Dots)){
    slot(x, i) <- Dots[[i]]
  }
  x
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Update model's dataset
#'
#' getModelData updates the  currently specified or built model with most recent data.
#' getModelData is a re-implementation of the R CRAN package getModelData with added flexibility in source.envir and Dots.
#'
#' @details
#' \preformatted{
#' Primarily Used within specify model calls, getModelData is used
#' to retrieve the appropriate underlying variables, and
#' apply model specified transformations automatically.
#' It can be used to also update a current model in memory
#' with the most recent data.
#' }
#' @export
#' @param x Default is none. Required. An object of class quantmod.
#' @param na.rm	Logical. Defaults to TRUE. Remove NA values.
#' @param source.envir Default is NULL. First place to search for Symbols.
#' @param  ... Dots passed to getSymbolsEnv.
#' @return Returns object of class quantmod.OHLC
#' @author Andre Mikulec (re-implementation)
#' @author Jeffrey Ryan
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom xts xts as.xts
getModelData <- function (x, na.rm = TRUE, source.envir = NULL, ...) {
tryCatchLog::tryCatchLog({

  model <- x
  if (!is.quantmod(model))
    stop(sQuote("x"), "must be of class", dQuote("quantmod"),
         "\n")
  if (length(model@model.inputs) == 0) {
    build.vars <- c(model@model.target, model@build.inputs)
  }
  else {
    build.vars <- c(model@model.target, model@model.inputs)
  }
  model.symbols <- vars <- all.vars(model@model.spec)
  env <- new.env()
  lapply(vars, function(V) {
    if(is.null(source.envir)) {
      if(!exists(V)) {
        getSymbolsEnv(V, env = env, ...)
      }
      else {
        assign(V, get(V), env)
      }
    } else {
      if (!exists(V, envir = source.envir)) {
        getSymbolsEnv(V, env = env, ...)
      } else {
        assign(V, get(V, envir = source.envir), env)
      }
    }
  })
  target.data <- get(model.symbols[[1]], env)
  total.columns = NULL
  for (j in 1:length(model.symbols)) {
    if (j == 1) {
      m <- xts::as.xts(target.data)
    }
    else {
      m <- merge(m, xts::as.xts(get(model.symbols[[j]], env)),
                 join = "inner")
    }
    total.columns[j] <- ncol(m)
  }
  fullIndex <- index(m)
  from.col = 1
  for (i in 1:length(model.symbols)) {
    assign(model.symbols[[i]], m[, from.col:(total.columns[i])],
           env)
    from.col = total.columns[i] + 1
  }
  mf <- xts::xts(model.frame(model@model.spec, data = env, na.action = NULL),
            fullIndex)
  if (na.rm)
    mf <- rbind(na.exclude(mf[-nrow(mf), ]), mf[nrow(mf),
    ])
  colnames(mf) <- lapply(colnames(mf), function(x) {
    gsub("[) ]", "", gsub("[(,=^:'\"]", ".", x))
  })
  model@model.data <- mf
  model@build.inputs <- colnames(mf)[-1]
  model@model.formula = as.formula(paste(colnames(mf)[1], "~",
                                         paste(colnames(mf)[-1], collapse = "+"), sep = ""))
  return(model)
})}



#' Specify Model Formula For quantmod Process
#'
#' @description
#' \preformatted{
#'
#' specifyModel a re-implmentation of the
#' R Package quantmodel specifyModel.
#'
#' specifyModel, creates a single reusable model specification
#' for subsequent buildModel calls. An object of class quantmod
#' is created that can be then be reused with
#' different modeling methods and parameters.
#' No data frame is specified, as data is retrieved from potentially
#' multiple environments, and internal calls to getSymbolsEnv.
#'
#' (1) The new parameter source.envir exists and is
#' passed to the (also) re-implmented R CRAN pacakge
#' quantmod function getModelData.
#'
#' getSymbolsEnv searches first in (source.envir) to acquire Symbols.
#' If the Symbol is not found in the enviroment (source.envir),
#' then get the Symbol from elsewhere.
#'
#' (2) The new parameter ... Dots is passed to passed to
#' the function getModelData (that are eventually passed
#' to getSymbolsEnv).
#'
#' The situation may be useful that the caller setup source lookups
#' of specific Symbols using src (and yahoo case "from" and "to")
#' using the R CRAN package quantmod function setSymbolLookup.
#' }
#' @details
#' \preformatted{
#' Models are specified through the standard formula mechanism.
#'
#' As financial models may include a variety of financial and economic
#' indicators, each differing in source, frequency, and/or class,
#' a single mechanism to specify sources is included within a call
#' to specifyModel. See getModelData for details of how this
#' process works.
#'
#' Currently, objects of class quantmod.OHLC, zoo and ts
#' are supported within the model formula.
#'
#' All symbols are first (attempted to be ) retrieved from the
#' the source.envir followed by the global environment,
#' without inheritence. If an object is not found in the
#' global environment, it is added to a list of objects to load
#' through the getSymbolsEnv function. getSymbolsEnv retrieves each
#' object specified by using information as to its location
#' specified apriori via setDefaults or setSymbolLookup.
#'
#' Internally all data is coerced to zoo, data.frame, or
#' numeric classes.
#' }
#' @note
#' \preformatted{
#' It is possible to include any supported series in the formula by
#' simply specifying the object's symbol. See *Details* for a list
#' of currently supported classes.
#'
#' Use getSymbols.skeleton to create additional methods of
#' data sourcing, e.g. from a proprietary data format or
#' currently unimplemented source (Bloomberg, Oracle).
#'
#' See getSymbols.MySQL and getSymbols.yahoo for examples
#' of adding additional functionality.
#' }
#' @examples
#' \dontrun{
#' # list of Symbols
#' Symbols <- unlist(lapply( c("MSFT","AAPL","WMT","COST"),
#'              function(x) {
#'                l <- list()
#'                l[[x]] <- getSymbolsEnv(x, auto.assign = FALSE)
#'              }), recursive = FALSE)
#'
#' # environment of Symbols
#' Symbols <- list2env(Symbols)
#'
#' # see the Symbols
#' ls.str(Symbols)
#'
#' # acquire the Symbols and return them (to this frame)
#' # getSymbolsEnv(c("AAPL","ORCL"), source.envir = Symbols)
#' # delete (not used)
#' # rm("AAPL","ORCL")
#'
#' # This example uses R CRAN package econModel functions
#' # LD(lead) and LG(lag).
#' # Symbols are acquired from the environment Symbols.
#' # This call tries to acquire Symbols
#' # from the "source.envir" Symbols.
#' quantmod <- specifyModel(LD(quantmod::ClCl(WMT)) ~
#'       LG(quantmod::OpCl(AAPL)) + LG(quantmod::LoHi(COST),0:2),
#'     source.envir = Symbols,
#'     from = "2007-01-01" # passed to the function getSymbolsEnv
#'     to   = "2011-12-31" # passed to the function getSymbolsEnv
#'   )
#' }
#' @param formula R formula. Default is none. Required. An object of class formula (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specifcation are given under Details.
#' @param na.rm Logical. Default is TRUE. Remove all incomplete rows. If na.rm is TRUE, then "na.exclude" is Done. "Without rules" this puts back (rbind) the last observation.
#' @param source.envir Environment.  Default is NULL.
#' @param ... Dots passed to getModelData.
#' @return Returns an object of class quantmod. Use modelData to extract full data set as zoo object.
#' @author Andre Mikulec (the re-impemtation)
#' @author Jeffrey Ryan
#' @importFrom tryCatchLog tryCatchLog
#' @export
specifyModel <- function (formula, na.rm = TRUE, source.envir = NULL, ...) {
tryCatchLog::tryCatchLog({

  new.quantmod <- new("quantmod")
  formula <- as.formula(formula)
  dot.vars <- all.vars(formula)
  convert.vars <- function(vars) {
    v <- unlist(strsplit(vars, "[.]"))
    v <- paste(v[1], "(", v[2], if (length(v) > 2)
      paste(",", v[3], sep = ""), ")", sep = "")
    return(v)
  }
  new.quantmod@model.spec <- formula
  new.quantmod@model.formula <- as.formula(gsub("[) ]", "",
                                                gsub("[(,=:^'\"]", ".", deparse(formula))))
  new.quantmod@model.target <- as.character(new.quantmod@model.formula[[2]])
  new.quantmod@build.inputs <- as.character(attr(terms(new.quantmod@model.formula),
                                                 "term.labels"))
  vars <- all.vars(formula)
  new.quantmod@symbols <- vars
  new.quantmod@product <- vars[1]
  new.quantmod <- getModelData(new.quantmod, na.rm = na.rm, source.envir = source.envir, ...)
  return(new.quantmod)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' check the .libPaths() to see a package has a training function
#'
#' Extracted because the R CRAN package quantmod does not export this.
#'
#' @param method String. No Default. Required. Name of a R package function.
#' @param package String. No Default. Required. Name of a R package
#' @return Logical. Availablity of a function in an R package
#' @rdname is.method.available
#' @importFrom tryCatchLog tryCatchLog
quantmod___is.method.available <- function (method, package) {
tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  if (!package %in% .packages()) {
    if (package %in% .packages(all.available = TRUE)) {
      cat(paste("loading required package:", package, "\n"))
      library(package, character.only = TRUE)
    }
    else {
      stop(paste("package", sQuote(package), "containing",
                 sQuote(method), "unable to be located"))
    }
  }
  return(TRUE)
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}
#' @rdname is.method.available
#' @importFrom tryCatchLog tryCatchLog
#' @export
is.method.available <- function(method, package) {
tryCatchLog::tryCatchLog({

  quantmod___is.method.available(method, package = package)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Build quantmod model given specified fitting method
#'
#' Construct and attach a fitted model of type method to quantmod object.
#' This is a re-implementation of the R CRAN package buildModel with added flexibility in training.per
#'
#' @param x An object of class quantmod.  Required.  Created with specifyModel or an R formula
#' @param training.per Default is none. Required. Default will train on all data provided in the paramter object "quantmod".  Character vector representing dates in ISO 8601 format “CCYY-MM-DD” or “CCYY-MM-DD HH:MM:SS” of length 2.  training.per can also be a list of training dates (THE LIST FEATURE NOT YET IMPLMENTED).
#' @param method_train Default is none. Required. A character string naming the R CRAN package caret function train fitting method.
#' @param ... Dots.  Additional arguments to method call.
#' @return An object of class quantmod with fitted model attached.
#' @examples
#' \dontrun{
#' # of variable "specmodel", see
#' ? econModel::specifyModel
#'
#' # built model
#' specmodel <- buildModel(specmodel, method="train"
#'   , training.per=list(c("1970-12-31","1998-12-31"), c("2001-0101","2008-06-30"))
#' }
#' @importFrom zoo index
#' @importFrom DescTools DoCall
#' @export
buildModel <- function (x, method, training.per, ...) {
tryCatchLog::tryCatchLog({

  as.POSIXorDate <- function(x) {
    class.of.index <- class(index(model.data))
    if ("POSIXt" %in% class.of.index) {
      if ("POSIXlt" %in% class.of.index) {
        x <- as.POSIXlt(x)
      }
      else {
        x <- as.POSIXct(x)
      }
    }
    else {
      x <- as.Date(x)
    }
    x
  }
  model.id = deparse(substitute(x))
  model.data <- x@model.data

  # traditional method
  if(is.vector(training.per)) {
    if (length(training.per) != 2) {
      stop("training.per must be of length 2")
    }
    start.date.index <- index(model.data[which(zoo::index(model.data) >=
                                                 as.POSIXorDate(training.per[1]))])
    end.date.index <- index(model.data[which(zoo::index(model.data) <=
                                               as.POSIXorDate(training.per[2]))])

    training.dates <- as.POSIXorDate(intersect(as.character(start.date.index),
                                               as.character( end.date.index)))
  }
  # "new" additional flexibility
  if(is.list(training.per)) {
    training.dates <- lapply(training.per, function(training.per) {
      if (length(training.per) != 2) {
        stop("training.per must be of length 2")
      }
      start.date.index <- index(model.data[which(index(model.data) >=
                                                   as.POSIXorDate(training.per[1]))])
      end.date.index <- index(model.data[which(index(model.data) <=
                                                 as.POSIXorDate(training.per[2]))])

      training.dates <- as.POSIXorDate(intersect(as.character(start.date.index),
                                                 as.character( end.date.index)))
      training.dates
    })
  }
  # Dates with lapply and sapply
  # https://stackoverflow.com/questions/14449166/dates-with-lapply-and-sapply
  training.dates <- DescTools::DoCall(c, training.dates)

  method <- as.character(paste("buildModel.", method, sep = ""))
  training.data <- model.data[training.dates]
  formula <- x@model.formula
  mcall <- DescTools::DoCall(method, list(quantmod = x, training.data = training.data,
                                ...))
  x@fitted.model <- mcall$fitted
  x@model.inputs <- as.character(mcall$inputs)
  x@build.date = as.character(Sys.time())
  x@model.id <- paste(class(mcall$fitted)[length(class(mcall$fitted))],
                      as.numeric(Sys.time()), sep = "")
  x@training.data <- (training.dates)
  invisible(x)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Build a CRAN package quantmod "train" Model
#'
#' @description
#' \preformatted{
#'
#' # xgboost parameters
#' # https://xgboost.readthedocs.io/en/latest/parameter.html
#'
#' # tuneGrid (production)
#'
#' tg <- expand.grid(
#'  nrounds   =  c(500,200,100),
#'  eta       =  c(0.3,0.1,0.01,0.001),
#'  max_depth =  c(4,6,8,10),
#'  gamma     =  0,
#'  colsample_bytree = c(1,0.5),
#'  min_child_weight = 1,
#'  subsample        = c(1,0.5)
#' )
#'
#' # tuneGrid (non-production)
#'
#' tg <- expand.grid(
#'   nrounds   =  50,
#'   eta       =  c(0.1,0.01),
#'   max_depth =  c(4,7,10),
#'   gamma     =  0,
#'   colsample_bytree = 1,
#'   min_child_weight = 1,
#'   subsample        = 1
#' )
#'
#' tc <- caret::trainControl(method = "cv", number = 5, allowParallel = T)
#'
#' # of variable "specmodel", see
#' ? econModel::specifyModel
#'
#' # built model
#' specmodel <- buildModel(specmodel, method="train"
#'   , training.per=c("1970-12-31","2006-12-31")
#'   , method_train = "xgbTree", tuneGrid = tg, trControl = tc)
#' }
#' @rdname buildModel
#' @param method_train Default is none. Defaults to "xgboost". A character string naming the R CRAN package caret function train fitting method.
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom caret trainControl train
buildModel.train <- function(quantmod, training.data, ...) {
tryCatchLog::tryCatchLog({

  if(is.method.available("train","caret")) {

    Dots <- list()
    Dots <- c(Dots, list(...))
    DotsOrigNames <- names(list(...))

    # default
    if(!"method_train" %in% DotsOrigNames) {
      # Different from the R CRAN package caret defaults
      Dots[["method"]] <- "xgbTree"
    } else {
      Dots[["method_train"]] <- NULL
      Dots[["method"]] <- list(...)[["method_train"]]
    }

    # default
    if(!"trControl" %in% DotsOrigNames) {
      # Different from the R CRAN package caret defaults
      trControl <- caret::trainControl(method = "cv", number = 5)
      Dots[["trControl"]] <- trControl
    }

    # default
    if(!"tuneGrid" %in% DotsOrigNames) {
      # Different from the R CRAN package caret defaults
      # tuneGrid (sample) # and defaults
      tuneGrid <- expand.grid(
        nrounds   =  100,
        eta       =  c(0.1,0.01), # default 0.3
        max_depth =  c(4,6,8,10), # default 6
        gamma     =  0,           # default 0
        colsample_bytree = c(1,0.5), # default 1
        min_child_weight = 1,        # default 1
        subsample        = c(1,0.5)  # default 1
      )
      Dots[["tuneGrid"]] <- tuneGrid
    }

    if(!all(complete.cases(training.data))) {
      message("NOTE: in buildModel.train, training.data is missing some data: caret::train.train WILL NOT fail.")
    }

    # DescTools::DoCall
    #   Error in model.frame.default(form = <formula> +  invalid type (closure) for variable '(weights)'
    # I do not know why this Error occurs:
    # sometime, I will try to figure this out
    rp <- suppressWarnings(do.call(caret::train, base::append(c(list(), list(quantmod@model.formula),data=list(training.data)), Dots[!names(Dots) %in% "stage"]) ) )

    return(list("fitted"=rp, "inputs"=attr(terms(rp),"term.labels")))
  } else {
    stop("method \"train\" is not available")
    return(NULL)
  }
})}




#' Predict
#'
#' Given an R Object and new data make predictions
#'
#' @param object R Object. Default is none. Required. Machine Learning object.
#' @param data  New Data.  Default is none. Required.
#' @param ... Dots Passed.
#' @return Prediction.
#' @rdname predictModel
#' @export
predictModel.default <- function (object, data, ...) {
  predict(object, data, ...)
  NextMethod("predictModel")
}



#' Predict
#'
#' Predict using the R CRAN package Caret class "train"
#'
#' @param object R Object. Default is none. Required. Machine Learning object.
#' @param data  New Data.  Default is none. Required.
#' @param ... Dots Passed.
#' @return Prediction.
#' @importFrom tryCatchLog tryCatchLog
#' @export
predictModel.train <- function (object, data, ...) {
tryCatchLog::tryCatchLog({
  if (is.method.available("train","caret")) {
    predict(object, data, ...)
  }
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}
