
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
#' @export
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
})}



#' copy Directory using a Regular Expression
#'
#' @description
#' \preformatted{
#' Code copy of R.utils::copyDirectory.default with the addition of the
#' parameter (and feature) "tolower".  The function is renamed to
#' be R.utils__copyDirectoryByPattern
#' }
#' @inheritParams R.utils::copyDirectory.default
#' @param tolower default is F. Convert the target file names to lower case.
#' @inherit R.utils::copyDirectory.default return details
#' @examples
#' \dontrun{
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom R.utils isDirectory Arguments isFile filePath
#' @export
R.utils__copyDirectoryByPattern <- function(from, to=".", ...,
                                   private=TRUE, recursive=TRUE,
                                   pattern = NULL, tolower = FALSE) {
tryCatchLog::tryCatchLog({

  # BACKWARD COMPATIBILITY: file.copy() gained argument copy.mode=TRUE in
  # R (>= 2.13.0) [April 2013].  Due to the default, this means that when
  # previously copying a read-only file, the new file would have write
  # permissions, whereas now it preserved the read-only permissions.
  # This private function silently drop argument 'copy.mode' and 'copy.date'
  # if passed older versions of R.
  .file.copy <- function(...) {
    args <- list(...)
    names <- names(args)
    if (!is.null(names)) {
      known <- names(formals(file.copy))
      keep <- (nchar(names) == 0L | is.element(names, known))
      args <- args[keep]
    }
    do.call(file.copy, args=args, envir=parent.frame())
  } # .file.copy()

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'from':
  if (!R.utils::isDirectory(from))
    throw("Argument 'from' is not a directory: ", from);

  # Argument 'to':
  to <- R.utils::Arguments$getWritablePath(to, mkdirs=TRUE, absolutePath=FALSE);

  # Argument 'private':
  private <- R.utils::Arguments$getLogical(private);

  # Argument 'recursive':
  recursive <- R.utils::Arguments$getLogical(recursive);

  # Use relative pathnames
  files <- list.files(from, all.files=private, pattern = pattern, full.names=FALSE);
  files <- files[!basename(files) %in% c(".", "..")];
  files <- file.path(from, files);

  copiedFiles <- c();
  for (file in files) {
    basename <- basename(file);
    if (R.utils::isFile(file)) {
      if (.file.copy(from=file, to=R.utils::filePath(to, if(!tolower) { basename } else { tolower(basename) } ), ...)) {
        copiedFiles <- c(copiedFiles, file);
      }
    } else if (R.utils::isDirectory(file)) {
      if (recursive) {
        copiedFiles <- c(copiedFiles,
                         copyDirectoryByPattern(file, to=R.utils::filePath(to, basename), ..., recursive=TRUE));
      }
    }
  }

  invisible(copiedFiles);
})}
