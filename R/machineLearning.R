

#' Create/Remove More or Less Observations
#'
#' @description
#' From an xts object, produce more or less jittered or duplicate nearby observations.
#' The workhorse package here is the R CRAN package UBL (Utility Based Learning) and its *Regress functions.
#' This is a smart(er) wrapper.
#' \preformatted{
# # R CRAN package UBL *Regress functions as of November 2020
#'
#' GaussNoiseRegress : function (form, dat, rel = "auto", thr.rel = 0.5,
#'                               C.perc = "balance", pert = 0.1, repl = FALSE)
#'
#' # default
#' # from current data, makes "exact replicated" copies
#' ImpSampRegress : function (form, dat, rel = "auto", thr.rel = NA,
#'                            C.perc = "balance", O = 0.5, U = 0.5)
#'
#' RandOverRegress : function (form, dat, rel = "auto", thr.rel = 0.5,
#'                             C.perc = "balance", repl = TRUE)
#'
#' # from current data, makes "jittered" copies
#' RandUnderRegress : function (form, dat, rel = "auto", thr.rel = 0.5,
#'                              C.perc = "balance", repl = FALSE)
#'
#' SmoteRegress : function (form, dat, rel = "auto", thr.rel = 0.5,
#'                          C.perc = "balance", k = 5, repl = FALSE,
#'                          dist = "Euclidean", p = 2)
#'
#' UtilOptimRegress : function (form, train, test, type = "util",
#'                              strat = "interpol",
#'                              strat.parms = list(method = "bilinear"),
#'                              control.parms, m.pts, minds, maxds, eps = 0.1)
#'
#' # Help with UtilOptimRegress(just above) parameter control.parms
#'
#'     phi.control : function (y, method = "extremes", extr.type = "both",
#'                             coef = 1.5, control.pts = NULL)
#' }
#' @param x xts object of training data.  Default is none. Required.
#' @param x2 xts object of testing data.  Default is NULL. Required in UtilOptimRegress. Only used in UtilOptimRegress. Otherwise an error.
#' @param Fmla Default is NULL.  Required. Formula that is sent to the UBL function.
#' @param TrainDates Default is NULL. Not Required. Absolute training start dates(times) and end dates(times) as a vector of a pair. Alternately, this can be a list of vectors of pairs.
#' @param TestDates Default is NULL. Not Required. This parameter can only be used with UtilOptimRegress. Absolute testing start dates(times) and end dates(times) as a vector of a pair. Alternately, this can be a list of vectors of pairs.
#' @param UBLFunction Default is NULL. Default is the ImpSampRegress function. Not Required. An R Package UBL *Regress function.
#' Enter the functoin name enclosed in a "string" or bare function name.
#' @param ... Dots passed to the UBL function.  Defaults follow.
#' thr.rel = 0.5.  C.perc = list(1, 2) : means make the important data to be from single in size to double in size.
#' Relevance function (rel): xts coredata values greater than zero are important. In opposite, xts coredata values less than zero are not important.
#' @return Modified xts that ahs removed data and/or has duplicate(multiplicate) index items at the same time points in time with the "jittered" coredata values or "exact replicated" coredata values.
#' @references
#' \cite{SmoteRegress challenges #2
#' \url{https://github.com/paobranco/UBL/issues/2}
#' }
#' @references
#' \cite{question about new/replicated UBL data and range of creation area #3
#' \url{https://github.com/paobranco/UBL/issues/3}
#' }
#' @references
#' \cite{P. Branco, L. Torgo and R.P. Ribeiro, Pre-processing approaches for imbalanced distributions in regression, Neurocomputing,
#'   \url{https://doi.org/10.1016/j.neucom.2018.11.100}
#'   \url{https://web.cs.dal.ca/~branco/PDFfiles/j14.pdf}
#' }
#' @references
#' \cite{Volume 74 by the Proceedings of Machine Learning Research on 11 October 2017
#'   \url{https://github.com/mlresearch/v74}
#' }
#' @references
#' \cite{(BROKEN LINK) Luis Torgo: Learning with Imbalanced Domains, a tutorial, 2nd International Workshop on Learning with Imbalanced Domains: Theory and Applications Co-located with ECML/PKDD 2018
#' \url{http://lidta.dcc.fc.up.pt/Slides/TutorialLIDTA.pdf}
#' }
#' @references
#' \cite{Paula Branco, Rita P. Ribeiro, Luis Torgo: UBL: an R package for Utility-based Learning, (Submitted on 27 Apr 2016 (v1), last revised 12 Jul 2016 (this version, v2))
#' \url{https://arxiv.org/abs/1604.0807}
#' }
#' @references
#' \cite{Ribeiro, R.P.: Utility-based Regression. PhD thesis, Dep. Computer Science, Faculty of Sciences - University of Porto (2011), Chapter 3 Utility-based Regression
#' \url{https://www.dcc.fc.up.pt/~rpribeiro/publ/rpribeiroPhD11.pdf}
#' }
#' @examples
#' \dontrun{
#' set.seed(1L)
#' DataValues <- data.frame(x = as.numeric(seq_len(1000)), y = rnorm(1000, 0, 1))
#' row.names(DataValues) <- seq_len(1000)
#'
#' table(DataValues$y > 0.00)
#' FALSE  TRUE
#' 518   482
#'
#' # Relevance function
#' Rlvce <- matrix(c(-0.01, 0, 0, 0.00, 0.5, 0.5, 0.01, 1, 0), ncol = 3, byrow = T,
#'                 dimnames = list(
#'                   yvalues = character(),
#'                   col = c("yvalues", "relevance", "slope_of_y_values")
#'                 )
#' )
#'
#' # Relevant observations: import to me.
#' # I want MORE of these "relevant" observations
#' # (compared to "not very relevant" observations.)
#' #
#' # yvalues: negative(-) values are not VERY relevant
#' # yvalues: positive(+) values are VERY relevant
#' # relevance column:  0 - not very relevant, 1 - very relevant
#' #
#' # Relevance function defines a graphic with a smooth non-strait line
#' # It uses exactly only: yvalues and slope_of_yvalues
#' # see the references.
#' # This Relevance function is a curved line of half of a hill.
#' #
#' Rlvce
#' # +/-
#' col
#' yvalues yvalues relevance slope_of_y_values
#' [1,]   -0.01       0.0               0.0 # yvalues less than thr.rel (bottom of hill)
#' [2,]    0.00       0.5               0.5 # relevance col: thr.rel = 0.5
#' [3,]    0.01       1.0               0.0 # yvalues greater than thr.rel (top of hill)
#'
#' # default "threashold of relevance" (thr.rel) between "not very relevant" and "relavent"
#' # ranges
#' # "thr.rel = 0.5"                                                # [1,]->[2,] [2,]->[3,]
#' Results <- UBL::SmoteRegress(y ~ ., DataValues, rel = Rlvce, C.perc = list(0.5, 2.5))
#'
#' # no change
#' Results <- UBL::SmoteRegress(y ~ ., DataValues, rel = Rlvce, C.perc = list(1, 1))
#' > identical(sort(DataValues[,"x"]), sort(Results[,"x"]))
#' [1] TRUE
#' > identical(sort(DataValues[,"y"]), sort(Results[,"y"]))
#' [1] TRUE
#'
#' # new jitters of the current data
#' #
#' # double the number of (important) revelant observations
#' # default "thr.rel = 0.5"
#' #                                           # 100% percent, # 200% percent
#' Results <- UBL::SmoteRegress(y ~ ., DataValues, rel = Rlvce, C.perc = list(1, 2))
#'
#' table(Results$y > 0.00)
#' FALSE  TRUE
#' 518   964
#'
#' # new replicas of the current data
#' #
#' # default "thr.rel = NA" # to create/destroy obs like smote (thr.rel = 0.5)
#' Results <- UBL::ImpSampRegress(y ~ ., DataValues, rel = Rlvce, thr.rel = 0.5, C.perc = list(1, 2))
#' table(Results$y > 0.00)
#' FALSE  TRUE
#' 518   964
#'
#' # see the replicated data points
#' tail(Results[order(Results$x),],30)
#' # Results[order(as.integer(row.names(Results))),]
#'
#' # half the number of (un-important) not very relevant observations
#' #
#' Results <- UBL::SmoteRegress(y ~ ., DataValues, rel = Rlvce, C.perc = list(0.5, 1))
#' table(Results$y > 0.00)
#' FALSE  TRUE
#' 259   482
#'
#' Results <- UBL::ImpSampRegress(y ~ ., DataValues, rel = Rlvce, thr.rel = 0.5, C.perc = list(0.5, 1))
#' table(Results$y > 0.00)
#' FALSE  TRUE
#' 259   482
#'
#' # xts object
#'
#' DataIndex  <- zoo::as.Date(0L:999L)
#' DataXts <- xts::as.xts(DataValues, DataIndex, dateFormat= "Date")
#' table(DataXts[,"y"] > 0.00)
#'
#' # double the "important" data (jitters)
#' ResultsXts <- rebalanceData(y ~ ., DataXts, UBLFunction = "UBL::SmoteRegress")
#' table(ResultsXts[,"y"] > 0.00)
#'
#' # double the "important" data (exact data)
#' ResultsXts <- rebalanceData(y ~ ., DataXts)
#' table(ResultsXts[,"y"] > 0.00)
#'
#' # half the "not important" data
#' ResultsXts <- rebalanceData(y ~ ., DataXts, C.perc = list(0.5, 1))
#' table(ResultsXts[,"y"] > 0.00)
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom UBL GaussNoiseRegress ImpSampRegress RandOverRegress RandUnderRegress SmoteRegress UtilOptimRegress
#' @importFrom DescTools DoCall
#' @importFrom zoo index
#' @importFrom xts xts as.xts  first last
#' @importFrom xts tclass `tclass<-` tformat `tformat<-` tzone `tzone<-`  xtsAttributes `xtsAttributes<-`
#' @export
rebalanceData <- function(
  x,
  x2 = NULL,
  Fmla = NULL,
  TrainDates = NULL,
  TestDates = NULL,
  UBLFunction = NULL,
  ...
) {
tryCatchLog::tryCatchLog({

  if(!is.null(x)) {
    Data <- xTs <- x
    if(is.null(TrainDates)) {
      TrainDates <- c(xts::first(index(Data)), xts::last(inde(Data)))
    }
    # data may contain incomplete cases
    if (!is.null(TrainDates)) {
      if(is.vector(TrainDates)) {
        if (length(TrainDates) > 2) {
          OrigData <- Data[index(Data) %in% TrainDates]
        }
        else {
          start.date.index <- index(Data[which(index(Data) >=  TrainDates[1])])
          end.date.index   <- index(Data[which(index(Data) <=  TrainDates[2])])
          date.range       <- intersect(start.date.index, end.date.index)
          OrigData <- Data[date.range]
        }
      }
      # "new" additional flexibility
      if(is.list(TrainDates)) {
        OrigData <- lapply(TrainDates, function(x) {
          if (length(TrainDates) > 2) {
            OrigData <- Data[index(Data) %in% TrainDates]
          }
          else {
            start.date.index <- index(Data[which(index(Data) >= TrainDates[1])])
            end.date.index   <- index(Data[which(index(Data) <= TrainDates[2])])
            date.range       <- intersect(start.date.index, end.date.index)
            OrigData <- Data[date.range]
          }
          OrigData
        })
        # Dates with lapply and sapply
        # https://stackoverflow.com/questions/14449166/dates-with-lapply-and-sapply
        OrigData <- DescTools::DoCall(c, OrigData)
      }
    }
  }

  if(!is.null(x2)) {
    Data2 <- xTs2 <- x2
    if(is.null(TestDates)) {
      TestDates <- c(xts::first(index(Data2)), xts::last(inde(Data2)))
    }
    # data may contain incomplete cases
    if (!is.null(TestDates)) {
      if(is.vector(TestDates)) {
        if (length(TestDates) > 2) {
          OrigData2 <- Data2[index(Data2) %in% TestDates]
        }
        else {
          start.date.index <- index(Data2[which(index(Data2) >=  TestDates[1])])
          end.date.index   <- index(Data2[which(index(Data2) <=  TestDates[2])])
          date.range       <- intersect(start.date.index, end.date.index)
          OrigData2 <- Data2[date.range]
        }
      }
      # "new" additional flexibility
      if(is.list(TestDates)) {
        OrigData2 <- lapply(TestDates, function(x) {
          if (length(TestDates) > 2) {
            OrigData2 <- Data2[index(Data2) %in% TestDates]
          }
          else {
            start.date.index <- index(Data2[which(index(Data2) >= TestDates[1])])
            end.date.index   <- index(Data2[which(index(Data2) <= TestDates[2])])
            date.range       <- intersect(start.date.index, end.date.index)
            OrigData2 <- Data2[date.range]
          }
          OrigData2
        })
        # Dates with lapply and sapply
        # https://stackoverflow.com/questions/14449166/dates-with-lapply-and-sapply
        OrigData2 <- DescTools::DoCall(c, OrigData2)
      }
    }
  }

  if(!is.null(UBLFunction)) {
    if(mode(UBLFunction) == "function") {
      UBLFunction = match.fun(UBLFunction)
    } else {
      UBLFunction <- UBLFunction
    }
  } else {
    # default
    # UBL::ImpSampRegress: The relevance function is used to introduce
    # replicas of the most important examples and to remove the least important examples.
    # ? UBL::ImpSampRegress
    # WERCS: WEighted Relevance-based Combination Strategy
    UBLFunction <- UBL::ImpSampRegress
  }

  Dots <- c(list(),list(...))

  UBLData <- cbind(as.data.frame(OrigData), index = as.POSIXct(zoo::index(OrigData)))
  row.names(UBLData) <- NULL
  UBLData <- UBLData[complete.cases(UBLData),,drop = FALSE]
  # common case
  if(! identical(UBLFunction, UBL::UtilOptimRegress ) || identical(UBLFunction, "UtilOptimRegress")) {
    if(!is.null(x2)) stop("Using any *Regress function other than \"UtilOptimRegress\" does not use  \"test\" data(x2).")
    # add
    Dots <- append(Dots, list(dat = UBLData))
  }
  # UtilOptimRegress (unique case)
  if( identical(UBLFunction, UBL::UtilOptimRegress ) || identical(UBLFunction, "UtilOptimRegress")) {
    if(is.null(x2)) stop("Using \"UtilOptimRegress\" \"test\" data(x2) is required.")
    # add
    Dots <- append(Dots, list(train =  UBLData))
    # add
    UBLData2 <- cbind(as.data.frame(OrigData2), index = as.POSIXct(zoo::index(OrigData2)))
    row.names(UBLData2) <- NULL
    UBLData2 <- UBLData2[complete.cases(UBLData2),,drop = FALSE]
    Dots <- append(Dots, list(test = UBLData2))
    # remove
    Dots <- Dots[!Names(Dots) %in% "dat"]
  }

  if(is.null(Fmla)) stop("Formula \"Fmla\" is required.")
  # formula.tools:::as.character.formula
  UBLDataFormula <- as.formula(paste0(as.character(Fmla), " + index"))

  # values lhs of formula
  # with values LESS than zero are MORE relevant (e.g. [financial] losses)
  #
  # UBL relevance function
  #
  #   # to plot the relevance function, then use ONLY these two columns
  #   actual y-val                                     slope at height(y-axis)
  #   # columns of the relevance function
  #   actual y-val,  relevance(0 to 1) height(y-axis), slope at height(y-axis)

  if(!"rel" %in% Names(Dots)){
    rel <- matrix(c(
      -0.01, 1.0, 0.0, # negative y-values ( I care *much* about -> 1)
       0.00, 0.5, 0.5,
       0.01, 0.0, 0.0  # positive y-values ( I do not care *much* about -> 0 )
    ),
      ncol = 3,
      byrow = TRUE,
      dimnames = list(
        yvalues = character(),
        col = c("yvalues", "relevance", "slope_of_y_values")
      )
    )
    Dots[["rel"]] <- rel
  }

  # threshold
  if(!"thr.rel" %in% Names(Dots)) {
    thr.rel <- 0.5
    Dots[["thr.rel"]] <- thr.rel
  }

  if(!"C.perc" %in% Names(Dots)) {
    # Current data is 100% is 1.
    # Adjusted data is 200% percent is 2. (100% is 1 is the same old data)
    # C.perc = list(1, 2))
    C.perc = list(1, 2)
  } else {
    Dots[["C.perc"]] <- C.perc
  }

  if( identical(UBLFunction, UBL::UtilOptimRegress ) || identical(UBLFunction, "UtilOptimRegress")) {
    Dots <- Dots[!Names(Dots) %in% c("rel", "thr.rel", "C.perc")]
  }

  UBLResults <- DescTools::DoCall(UBLFunction, c(list(), list(form = UBLDataFormula), Dots))

  UBLResultsIndex <- UBLResults[["index"]]
  UBLResults      <- UBLResults[, !colnames(UBLResults) %in% "index" , drop = FALSE]

  # redefine
  AdjustedData <- xts::as.xts(as.matrix(UBLResults), order.by = as.POSIXct(UBLResultsIndex))
  xts::tclass(AdjustedData)  <- xts::tclass(xTs)
  xts::tformat(AdjustedData) <- xts::tformat(xTs)
  xts::tzone(AdjustedData) <- xts::tzone(xTs)
  # (+) non-core attributes (user) [if any]
  xts::xtsAttributes(AdjustedData) <- xts::xtsAttributes(xTs)

  # UBL functions will not leak data
  #   EXCEPT *GaussNoiseRegression* that can/will leak data.
  # SEE . . .
  # question about new/replicated UBL data and range of creation area #3
  # https://github.com/paobranco/UBL/issues/3
  #
  # GaussNoiseRegression: prevent any leaking of
  # any "new" [if any] UBL data into another/other zone
  # This is particularly harsh to GaussNoiseRegression
  if (!is.null(TrainDates)) {
    if(is.vector(TrainDates)) {
      if (length(TrainDates) > 2) {
        AdjustedData <- AdjustedData[index(AdjustedData) %in% TrainDates]
      }
      else {
        start.date.index <- index(AdjustedData[which(index(AdjustedData) >=  TrainDates[1])])
        end.date.index   <- index(AdjustedData[which(index(AdjustedData) <=  TrainDates[2])])
        date.range       <- intersect(start.date.index, end.date.index)
        AdjustedData <- AdjustedData[date.range]
      }
    }
    # "new" additional flexibility
    if(is.list(TrainDates)) {
      AdjustedData <- lapply(TrainDates, function(x) {
        if (length(TrainDates) > 2) {
          AdjustedData <- AdjustedData[index(AdjustedData) %in% TrainDates]
        }
        else {
          start.date.index <- index(AdjustedData[which(index(AdjustedData) >= TrainDates[1])])
          end.date.index   <- index(AdjustedData[which(index(AdjustedData) <= TrainDates[2])])
          date.range       <- intersect(start.date.index, end.date.index)
          AdjustedData <- AdjustedData[date.range]
        }
        AdjustedData
      })
      # Dates with lapply and sapply
      # https://stackoverflow.com/questions/14449166/dates-with-lapply-and-sapply
      AdjustedData <- DescTools::DoCall(c, AdjustedData)
    }
  }
  # NOTE: replicate index values (may) exist (ImpSampRegress)

  # Results are expected to be put into machine learning function quickly
  AdjustedData

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}




#' Load and Manage Data from Multiple Sources
#'
#' @description
#' \preformatted{
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
    } else if(is.list(Symbols)) {
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

    # In a vector, the "remaining Symbols" not found in the environment
    # current.symbols
    #
    if(is.character(Symbols)) {
      Symbols <- paste0(current.symbols, collapse = ";")
    } else if(is.list(Symbols)) {
      Symbols <- Symbols[names(Symbols) %in% current.symbols]
    }
  }

  if(auto.assign && !is.null(env)) {
    if(length(symbols.returned.from.envir)) {
      current.symbols_i.total.collection <- character()
      for(current.symbols_i in names(symbols.returned.from.envir)) {
        # assign to "env"
        assign(current.symbols_i, symbols.returned.from.envir[[current.symbols_i]], envir = env)
        current.symbols_i.total.collection <- c(current.symbols_i.total.collection, current.symbols_i)
      }
      # print the others that were returned from the environment
      print(current.symbols_i.total.collection)
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
    if(is.list(data.window)) {
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
      # Dates with lapply and sapply
      # https://stackoverflow.com/questions/14449166/dates-with-lapply-and-sapply
      model.data <- DescTools::DoCall(c, model.data)
    }
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
#' modelData(specmodel) <- list(
#'   training.data = zoo::index(as.xts(sample.matrix))
#' )
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
#' @importFrom DescTools DoCall
#' @importFrom xts xts as.xts
getModelData <- function (x, na.rm = TRUE, source.envir = NULL, ...) {
tryCatchLog::tryCatchLog({

  Dots <- c(list(), list(...))
  Dots <- append(Dots, list(source.envir = source.envir))
  # still used below and "out-of-Dots"
  # source.envir

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
        DescTools::DoCall(getSymbolsEnv, c(list(), list(V), list(env = env), Dots))
      }
      else {
        assign(V, get(V), env)
      }
    } else {
      if (!exists(V, envir = source.envir)) {
        DescTools::DoCall(getSymbolsEnv, c(list(), list(V), list(env = env), Dots))
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
        stop("training.per vector must be of length 2")
      }
      start.date.index <- index(model.data[which(index(model.data) >=
                                                 as.POSIXorDate(training.per[1]))])
      end.date.index <- index(model.data[which(index(model.data) <=
                                                 as.POSIXorDate(training.per[2]))])

      training.dates <- as.POSIXorDate(intersect(as.character(start.date.index),
                                                 as.character( end.date.index)))
      training.dates
    })
    # Dates with lapply and sapply
    # https://stackoverflow.com/questions/14449166/dates-with-lapply-and-sapply
    training.dates <- DescTools::DoCall(c, training.dates)
  } else {
    stop("training.per list (if exists) must be of length 1 or greater")
  }


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
#' tc <- caret::trainControl(method = "cv", number = 5, summaryFunction = tradeModelSummary)
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
#' @param method_train Default is none. Defaults to "xgbTree". A character string naming the R CRAN package caret function train fitting method.
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
#' Given an R Object and new data, make predictions.
#' Implemented here because R CRAN package quantmod
#' function predictModel is not exported.
#'
#' @param object R Object. Default is none. Required. Machine Learning object.
#' @param data  New Data.  Default is none. Required.
#' @param ... Dots Passed.
#' @return Prediction.
#' @rdname predictModel
#' @export
predictModel <- function (object, data, ...) {

  useMethod("predictModel")

}



#' Predict
#'
#' Given an R Object and new data, make predictions.
#' Implemented here because R CRAN package quantmod
#' function predictModel is not exported.
#'
#' @param object R Object. Default is none. Required. Machine Learning object.
#' @param data  New Data.  Default is none. Required.
#' @param ... Dots Passed.
#' @return Prediction.
#' @rdname predictModel
predictModel.default <- function (object, data, ...) {

  predict(object, data, ...)

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
predictModel.train <- function (object, data, ...) {
tryCatchLog::tryCatchLog({
  if (is.method.available("train","caret")) {
    predict(object, data, ...)
  }
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Simulate Trading of Fitted quantmod Object
#'
#' @description
#' \preformatted{
#'
#' NOT IMPLEMENTED YET
#'
#' Perform Realization from using the R CRAN package quantmod functions
#' of modeling. This is real world actual testing (of Prediction).
#' This is a re-implementation of the R CRAN package
#' quantmod function tradModel.  It uses the R CRAN package
#' PerformanceAnalytics function Return.portfolio to do the heavy work.
#' Several PerformanceAnalytics exported functions replace out
#' some or part of quantmod non-exported functions
#' tradeModel, modelReturn, allReturns, periodReturn, and stripModelData.
#'
#' This function subtracts off many features from the R CRAN package
#' quantmod function tradeModel.
#'
#' Simulated trading of fitted quantmod object.
#' Given a fitted model, tradeModel calculates the signal generated
#' over a given historical period, then applies specified
#' trade.rule to calculate a return.
#' }
#' @details
#' \preformatted{
#' Apply a newly constructed model from buildModel to a new dataset
#' to investigate the model's trading potential.
#'
#' At present all parameters are very basic. The near term changes
#' include allowing for a trade.rule argument to allow for a
#' dynamic trade rule given a set of signals.
#'
#' Additional the application of variable leverage and costs
#' will become part of the final structure.
#' }
#' @param x	quantmod object (from buildModel). Required.
#' @param signal.threshold Numeric vector. Default is c(0, 0). A numeric vector describing simple lower and upper thresholds before trade occurs.
#' @param trade.dates Default is NULL. Specific trade interval - defaults to the full dataset. This can be a vector of pairs.  This can be a list of vectors of pairs.
#' @param exclude.training Logical. Default is TRUE. Exclude the period trained on?
#' @param ... Dots pass to additional parameters needed by the underlying modeling function, if any.
#' @return A quantmodResults object. The realizations.
#' @examples
#' \dontrun{
#' tradeModel(specmodel)@returns
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo index
#' @importFrom xts xts
#' @importFrom PerformanceAnalytics Return.portfolio
#' @export
tradeModel <- function (x, signal.threshold = c(0, 0),
          trade.dates = NULL, exclude.training = TRUE, ...) {
tryCatchLog::tryCatchLog({

  stop("To be implemented in PerformanceAnalytics::Return.portfolio")

  quantmod <- getModelData(x)
  if (class(quantmod) != "quantmod")
    stop("model must be of class quantmod")

  if (!length(trade.dates)) {
    stop("trade.dates must exist and must be a vector of pairs \nor a list(of size 1 or greater) of vectors of pairs")
  }

  if(is.vector(trade.dates)) {
    if (!is.null(trade.dates) && length(trade.dates) < 2)
      stop("trade.dates must be of length 2")
  }

  if(is.list(trade.dates)) {
    test.results <- unlist(lapply(trade.dates, function(trade.dates) {
      if (!is.null(trade.dates) && length(trade.dates) < 2) {
        return(F)
      } else {
        return(T)
      }
    }))
    if(!all(test.results)) {
      stop("trade.dates vector must be of length 2")
    }
  }

  model.data <- modelData(quantmod, trade.dates, exclude.training = exclude.training)
  fitted.results <- predictModel(quantmod@fitted.model, model.data, ...)

  if (!inherits(fitted.reslts, "xts")) {
    fitted.results <- xts::xts(as.vector(fitted.results), zoo::index(model.data))
  }
  # if beyond the extremes (either case)
  signal.results <- ifelse(fitted.results < signal.threshold[1] |
                           fitted.results > signal.threshold[2],
                             ifelse(fitted.results > 0,  1,  # ideal to go long (invest)
                                                        -1), # ideal to short (invest)
                                                         0)  # do not invest

  # more here

  market.results <- model.data[,1]
  signal.results <- merge(market.zoo, signal.results)
  quantmodResults <- list(model = quantmod, signal = signal.results)

  modelReturn <- function(tR.results, trade.dates = NULL) {

    if(is.vector(trade.dates)) {
      if (!is.null(trade.dates) && length(trade.dates) < 2)
        trade.dates <- zoo::index(window(xts(, index(trade.signal[, 1])),
                            start = trade.dates[1], end = trade.dates[2]
        ))
    }
    if(is.list(trade.dates)) {
      trade.dates <- unlist(lapply(trade.dates, function(trade.dates) {
        trade.dates <- zoo::index(window(xts(, index(trade.signal[, 1])),
                                   start = trade.dates[1], end = trade.dates[2]
        ))
        trade.dates
      }))
      trade.dates <- order(unique(trade.dates))
    }

    # rem: in exponential math
    cash.returns <- xts::xts(matrix(seq_len(0,length(trade.dates)), ncol = 1, dimnames = list(character(), "cash")), order.by = trade.dates)

    # trade.signal[market.data, signal.results]
    trade.signal <- tR.results$signal

    asset.weights.invest.long  <- ifelse(trade.signal[trade.dates, 2] ==  1, 1.00, 0.00)
    asset.weights.invest.no    <- ifelse(trade.signal[trade.dates, 2] ==  0, 1.00, 0.00)
    asset.weights.invest.short <- ifelse(trade.signal[trade.dates, 2] == -1, 1.00, 0.00)

    asset.weights <- xts(cbind(asset.weights.invest.long, asset.weights.invest.no, asset.weight.invest.short), order.by = trade.dates)

    returns <- PerformanceAnalytics::Return.portfolio(
            # long (invest)               # no invest        # short (invest)
      merge(trade.signal[trade.dates, 1], cash.returns, -1 * trade.signal[trade.dates, 1]),
      weights =  asset.weights,
      value = 10000.00
    )

    model.results <- returns
    model.results[which(is.na(model.results))] <- 0
    model.cumret <- cumprod(1 + model.results)

    quantmodReturn <- new("quantmodReturn")
    quantmodReturn@returns <- model.cumret
    return(quantmodReturn)

  }

  model.returns <- modelReturn(quantmodResults,
                               trade.dates = trade.dates,
                               leverage = leverage)

  quantmodResults$return <- model.returns
  return(structure(quantmodResults, class = "quantmodResults"))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Calculates performance across resamples
#'
#' Given two numeric vectors, obs and pred, of data, the performance is calculated.
#'
#' @param data Data.frame. Required. A data frame with columns, obs and pred, for the observed and predicted outcomes. For metrics that rely on class probabilities, such as twoClassSummary, columns should also include predicted probabilities for each class. See the classProbs argument to R CRAN package caret function trainControl.
#' @param lev  Character vector of factors levels for the response. In regression cases, this would be NULL.  . Default is NULL.
#' @param model String. Default is NULL.  Required. A character string for the model name (as taken from the method argument of train).
#' @returns profit
#' @examples
#' \dontrun{
#' options(tradeModelSummaryDots = list(signal.threshold = c(0,0)))
#'
#' model <- caret::train(dat[,-1], dat[,1], metric="profit", maximize=TRUE,
#'   trControl = caret::trainControl(summaryFunction = tradeModelSummary)
#' )
#' }
#' @references
#' \cite{Zachary Mayer, Time series cross-validation 5, January 24, 2013
#' \url{https://www.r-bloggers.com/2013/01/time-series-cross-validation-5/}
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo as.Date
#' @importFrom xts as.xts
#' @export
tradeModelSummary <- function(data, lev = NULL, model = NULL) {
tryCatchLog::tryCatchLog({

  cash.returns <- xts::xts(matrix(seq_len(0,NROW(dat)), ncol = 1, dimnames = list(character(), "cash")), order.by = zoo::as.Date(seq_len(NROW(dat))))

                                              # arbitrary index
  pred <- xts::as.xts(dat[,  "obs", drop = F], index = zoo::as.Date(seq_len(NROW(dat))))
  pred <- xts::as.xts(dat[, "pred", drop = F], index = zoo::as.Date(seq_len(NROW(dat))))

  tradeModelSummaryDots <- getOption("tradeModelSummaryDots")

  signal.threshold <- tradeModelSummaryDots[["signal.threshold"]]

  signal.results <- ifelse(pred < signal.threshold[1] |
                           pred > signal.threshold[2],
                                       ifelse(pred > 0,  1,  # ideal to go long (invest)
                                                        -1), # ideal to short (invest)
                                                         0)  # do not invest

  asset.weights.invest.long  <- ifelse(signal.results ==  1, 1.00, 0.00)
  asset.weights.invest.no    <- ifelse(signal.results ==  0, 1.00, 0.00)
  asset.weights.invest.short <- ifelse(signal.results == -1, 1.00, 0.00)

  asset.weights <- xts(cbind(asset.weights.invest.long, asset.weights.invest.no, asset.weight.invest.short), order.by = zoo::as.Date(seq_len(NROW(dat))))

  returns <- PerformanceAnalytics::Return.portfolio(
          # long (invest)   # no invest      # short (invest)
    merge(obs,              cash.returns,    -1 * obs),
    weights =  asset.weights,
    value = 10000.00
  )

  model.results <- returns
  model.results[which(is.na(model.results))] <- 0
  model.cumret <- cumprod(1 + model.results)
  dollar.return <- model.cumret * 100000.00

  profit <- dollar.return - 10000.00

  return(data.frame(cbind(profit)))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}
