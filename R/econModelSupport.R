
#' Convert a time series to an R CRAN package quantmod supported time series
#'
#' Used internally by the function getSymbols.ALFRED
#'
#' Replica of quantmod:::convert.time.series
#'
#' Code is repeated here because it is private code in the R CRAN package quantmod.  CRAN rules do not allow private functions in other packages to be called directly.
#'
#' @param fr see R CRAN package quantmod private function convert.time.series
#' @param return.class see R CRAN package quantmod private function convert.time.series
#' @return R CRAN quantmod compatible time series
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stats as.ts
#' @importFrom zoo as.zoo
#' @importFrom timeSeries timeSeries
quantmod___convert.time.series <- function (fr, return.class){
tryCatchLog::tryCatchLog({

  if ("quantmod.OHLC" %in% return.class) {
    class(fr) <- c("quantmod.OHLC", "zoo")
    return(fr)
  }
  else if ("xts" %in% return.class) {
    return(fr)
  }
  if ("zoo" %in% return.class) {
    return(zoo::as.zoo(fr))
  }
  else if ("ts" %in% return.class) {
    fr <- stats::as.ts(fr)
    return(fr)
  }
  else if ("data.frame" %in% return.class) {
    fr <- as.data.frame(fr)
    return(fr)
  }
  else if ("matrix" %in% return.class) {
    fr <- as.data.frame(fr)
    return(fr)
  }
  else if ("timeSeries" %in% return.class) {
    if (requireNamespace("timeSeries", quietly = TRUE)) {
      fr <- timeSeries::timeSeries(coredata(fr), charvec = as.character(index(fr)))
      return(fr)
    }
    else {
      warning(paste("'timeSeries' from package 'timeSeries' could not be loaded:",
                    " 'xts' class returned"))
    }
  }
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' copy Files and Directories Using a Regular Expression
#'
#' @description
#' \preformatted{
#' Code copy of R.utils::copyDirectory.default with the addition of the
#' features pattern(source files) CaseChange(destination files).
#' The function is renamed to be R.utils__copyDirectoryByPattern
#' }
#' @param from	The pathname of the source directory to be copied.
#' @param to	The pathname of the destination directory.
#' @param ...	Additional arguments passed to file.copy(), e.g. overwrite.
#' @param private	If TRUE, files (and directories) starting with a period is also copied, otherwise not.
#' @param recursive	If TRUE, subdirectories are copied too, otherwise not.  Note, the name of the subdirectory also must in in "pattern".
#' @param pattern regular expression of the names of the source files
#' @param CaseChange string. Default is NULL(no change).  Change to target file name to the desired case: NULL(no change), "UpperCase", "LowerCase".
#' @examples
#' \dontrun{
#'
#' # R.utils__copyDirectoryByPattern example
#'
#' R.utils__copyDirectoryByPattern("C:/Program Files (x86)/Stock Investor/Professional",
#'   to = tempdir(), pattern = "(*\\.dbf$|\\.*DBF$|\\.*DBF$|*.chm$|ReadMe\\.txt)",
#'   CaseChange = "UpperCase"
#' )
#' dir(tempdir())
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom R.oo throw
#' @importFrom R.utils isDirectory Arguments isFile filePath
R.utils__copyDirectoryByPattern <- function(from, to=".", ...,
                                   private=T, recursive=T,
                                   pattern = NULL, CaseChange = NULL) {
tryCatchLog::tryCatchLog({

  # BACKWARD COMPATIBILITY: file.copy() gained argument copy.mode=TRUE in
  # R (>= 2.13.0) [April 2013].  Due to the default, this means that when
  # previously copying a read-only file, the new file would have write
  # permissions, whereas now it preserved the read-only permissions.
  # This private function silently drop argument 'copy.mode' and 'copy.date'
  # if passed older versions of R.
  .file.copy <- function(...) {
    tryCatchLog::tryCatchLog({
    args <- list(...)
    names <- names(args)
    if (!is.null(names)) {
      known <- names(formals(file.copy))
      keep <- (nchar(names) == 0L | is.element(names, known))
      args <- args[keep]
    }
    do.call(file.copy, args=args, envir=parent.frame())
  }, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}
  # .file.copy()

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'from':
  if (!R.utils::isDirectory(from))
    R.oo::throw("Argument 'from' is not a directory: ", from);

  # Argument 'to':
  to <- R.utils::Arguments$getWritablePath(to, mkdirs=TRUE, absolutePath=FALSE);

  # Argument 'private':
  private <- R.utils::Arguments$getLogical(private);

  # Argument 'recursive':
  recursive <- R.utils::Arguments$getLogical(recursive);

  # NOTE: to copy a directory recursive-ly,
  # the name of the directory ALSO ALSO BE in "pattern"
  # Use relative pathnames
  files <- list.files(from, all.files=private, pattern = pattern, full.names=FALSE);
  files <- files[!basename(files) %in% c(".", "..")];
  files <- file.path(from, files);

  copiedFiles <- c();
  for (file in files) {
    basename <- basename(file);
    if (R.utils::isFile(file)) {
      TargetFileName <- basename
      if(!is.null(CaseChange)) {
        if(CaseChange == "UpperCase") {
          TargetFileName <- toupper(TargetFileName)
        }
        if(CaseChange == "LowerCase") {
          TargetFileName <- tolower(TargetFileName)
        }
      }
      if (.file.copy(from=file, to=R.utils::filePath(to, TargetFileName), ...)) {
        copiedFiles <- c(copiedFiles, file);
      }
    } else if (R.utils::isDirectory(file)) {
      if (recursive) {
        copiedFiles <- c(copiedFiles,
                         R.utils__copyDirectoryByPattern(file, to=R.utils::filePath(to, basename), ..., recursive=TRUE));
      }
    }
  }

  invisible(copiedFiles);
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}

#' x
#'
#' Of lsNamespaceInfo, from the name of a namespace, return a character vector of all objects
#'
#' @param ns String. Default none. Required. Name of a namespace
#' @param ... dots.  Passed to ls
#' @return Of lsNamespaceInfo, return a character vector of objects in a namespace
#' @author Andre Mikulec
#' @rdname unlockEnvironment
#' @importFrom tryCatchLog tryCatchLog
#' @export
lsNamespaceInfo <- function(ns, ...) {
tryCatchLog::tryCatchLog({
  ns <- asNamespace(ns, base.OK = FALSE)
  ls(..., envir = get(".__NAMESPACE__.", envir = ns, inherits = FALSE), all.names = T)
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


#' x
#'
#' Of AllInfoNS, from the name of a namespace, return information
#'
#' @param ns String. Default none. Required. Name of a namspace
#' @param ... dots.  Passed to ls
#' @return Of AllInfoNS, return lists and sublists of information about the namespace
#' @author Andre Mikulec is the author of AllInfoNS
#' @rdname unlockEnvironment
#' @examples
#' \dontrun{
#' #
#' # something not-on-the-search() path
#' #
#' requireNamespace("RPostgreSQL")
#'
#' getNamespace("RPostgreSQL")
#' <environment: namespace:RPostgreSQL>
#'
#' environmentIsLocked(getNamespace("RPostgreSQL"))
#' [1] TRUE
#'
#' unlockEnvironment(getNamespace("RPostgreSQL"))
#' [1] TRUE
#'
#' forceAssignInNamespace("prnt", function(x) {print(x)}, namespace = "RPostgreSQL")
#' RPostgreSQL::prnt("HelloEveryWhere")
#' Error: 'prnt' is not an exported object from 'namespace:RPostgreSQL'
#'
#' RPostgreSQL:::prnt("HelloEveryWhere")
#' [1] "HelloEveryWhere"
#'
#' assign("prnt","prnt", envir = AllInfoNS("RPostgreSQL")$exports)
#' RPostgreSQL::prnt("HelloEveryWhere")
#' [1] "HelloEveryWhere"
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @export
AllInfoNS <- function(ns) {
tryCatchLog::tryCatchLog({
  NamesNamespaceInfo <- lsNamespaceInfo(ns)
  x <- lapply(NamesNamespaceInfo, getNamespaceInfo, ns=ns)
  Names(x) <- NamesNamespaceInfo
  DescTools::DoCall(c, x)
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


#' x
#'
#' Of forceAssignInNamespace, force the assignment of a new item into a namespace.
#' Adapted from Rcpp. Based on the original function forceAssignMyNamespace by Willem Ligtenberg.
#'
#' @param x String. Required. Default none. Name of the symbol/function
#' @param value R Object. Default none. Required. New value of the parameter x symbol/function
#' @param namespace String. Default none. Required. Name of the namespace to assign parameter x into.
#' @return Of forceAssignInNamespace, silently assign object x (from parameter x) intoto the namespace non-exported objects collection
#' @author Willem Ligtenberg is the author of forceAssignInNamespace/forceAssignMyNamespace
#' @references
#' \cite{forceAssignMyNamespace by Willem Ligtenberg
#' \url{https://github.com/openanalytics/Rango/blob/adc99e077b71c8c6826cabb7ff1266050898718a/Rango/R/utils.R}
#' }
#' @examples
#' \dontrun{
#' #
#' # something on the search() path
#' #
#' library(RSQLite)
#'
#' getNamespace("RSQLite")
#' <environment: namespace:RSQLite>
#'
#' environmentIsLocked(getNamespace("RSQLite"))
#' [1] TRUE
#'
#' unlockEnvironment(asNamespace("RSQLite"))
#' [1] TRUE
#'
#' forceAssignInNamespace("prnt", function(x) {
#'   print(x)
#' }, namespace = "RSQLite")
#'
#' RSQLite::prnt("HelloEveryWhere")
#' Error: 'prnt' is not an exported object from 'namespace:RSQLite'
#'
#' RSQLite:::prnt("HelloEveryWhere")
#' [1] "HelloEveryWhere"
#'
#' # promote that function to be exported
#' assign("prnt","prnt", envir = AllInfoNS("RSQLite")$exports)
#' RSQLite::prnt
#' function(x) {print(x)}
#'
#' RSQLite::prnt("HelloEveryWhere")
#' [1] "HelloEveryWhere"
#' }
#' @useDynLib econModel
#' @rdname unlockEnvironment
#' @importFrom tryCatchLog tryCatchLog
#' @export
forceAssignInNamespace <- function(x, value, namespace){
tryCatchLog::tryCatchLog({
  if(x %in% ls(.getNamespace(namespace))){
    warning(paste0("Name clashes with an internal function name. Please use some other name."))
  }else{
    unlocker <- get("unlockBinding", baseenv())
    if(exists(x, envir = .getNamespace(namespace), inherits = FALSE) &&
       bindingIsLocked(x, .getNamespace(namespace))){
      unlocker(x, .getNamespace(namespace))
    }
    assign(x, value, .getNamespace(namespace))
    lockBinding(x, .getNamespace(namespace))
  }
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


#' Find All Substrings
#'
#' Given a string or vector of strings, find and extract all substrings that match the pattern.
#'
#' @param pattrn String. Regular Expression Pattern.
#' @param x vector of Strings.
#' @return vectors of found substrings
#' @references
#' \cite{Regular expression to extract text between square brackets
#' \url{https://stackoverflow.com/questions/2403122/regular-expression-to-extract-text-between-square-brackets}
#' }
#' @examples
#' \dontrun{
#' # not including the parentheses
#' regExtract("(?<=\\()(.*?)(?=\\))", "LIST (c1)")
#' [1] "c1"
#'
#' regExtract("(?<=\\()(.*?)(?=\\))", "LIST (((c1)::integer))")
#' [1] "((c1"
#'
#' RegExtract("(?<=\\()(\\w+)(?=\\))", "LIST (((c1)::integer))")
#' [1] "c1"
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
regExtract <- function(pattrn, x) {
tryCatchLog::tryCatchLog({
  Coordinates <- gregexpr(pattrn, x, perl = TRUE)
  lapply(Coordinates, function(Coord) {
    mapply(function(Co,Ma) {
      substr(x, Co, Co + Ma - 1L)
    }, Coord,  attr(Coord, "match.length"), SIMPLIFY = FALSE)
  }) -> Result
  return(unlist(Result))
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}
