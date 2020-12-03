## Startup functions ------------------------------------

#' .onAttach start message
#'
#' @param libname defunct
#' @param pkgname defunct
#' @return invisible()
#' @importFrom tryCatchLog tryCatchLog
.onAttach <- function(libname, pkgname) {
tryCatchLog::tryCatchLog({
  # NOV 2020
  # https://statacumen.com/teach/ShortCourses/R_Packages/R_Package_Development_20180817.html
  start_message <- c(   "\nProviding accesses to social and economic Data from ALFRED and elsewhere"
                      , "\nby Andre Mikulec\n"
  )
  packageStartupMessage(start_message)
  invisible()
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' .onLoad getOption package settings
#'
#' @param libname defunct
#' @param pkgname defunct
#'
#' @return invisible()
#'
#' @examples
#' getOption("econModel.name")
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom futile.logger flog.appender appender.file flog.threshold TRACE
.onLoad <- function(libname, pkgname) {
tryCatchLog::tryCatchLog({

  # NOV 2020
  # .onLoad
  # https://statacumen.com/teach/ShortCourses/R_Packages/R_Package_Development_20180817.html

  # NOTE: In R Studio, need to "restart R" to see changes
  #
  ops <- options()
  if(!"econModel.tryCatchLog.write.error.dump.folder" %in% Names(ops)) {
    options(append(ops, list(econModel.tryCatchLog.write.error.dump.folder = paste0(normalizePath(tempdir(), winslash = "/"), "/", "econModel.tryCatchLog.write.error.dump"))))
  }
  # # Sometimes useful with post-mortem debugging .rda file creation.
  # # .rda file creation requires:
  # options(tryCatchLog.write.error.dump.file = TRUE)
  if(!dir.exists(getOption("econModel.tryCatchLog.write.error.dump.folder"))) {
    dir.create(getOption("econModel.tryCatchLog.write.error.dump.folder"))
  }
  futile.logger::flog.appender(futile.logger::appender.file(paste0(getOption("econModel.tryCatchLog.write.error.dump.folder"), "/", "econModel.tryCatch.log")))
  futile.logger::flog.threshold(futile.logger::TRACE)   # TRACE, DEBUG, INFO, WARN, ERROR, FATAL

  invisible()
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}
