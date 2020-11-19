## Startup functions ------------------------------------

#' .onAttach start message
#'
#' @param libname defunct
#' @param pkgname defunct
#'
#' @return invisible()
.onAttach <- function(libname, pkgname) {
  # NOV 2020
  # https://statacumen.com/teach/ShortCourses/R_Packages/R_Package_Development_20180817.html
  start_message <- c(   "\nProviding accesses to social and economic Data from ALFRED and elsewhere"
                      , "\nby Andre Mikulec\n"
  )
  packageStartupMessage(start_message)
  invisible()
}


#' .onLoad getOption package settings
#'
#' @param libname defunct
#' @param pkgname defunct
#'
#' @return invisible()
#'
#' @examples
#' getOption("econModel.name")
#' @importFrom futile.logger flog.appender appender.file flog.threshold TRACE
.onLoad <- function(libname, pkgname) {
  # NOV 2020
  # https://statacumen.com/teach/ShortCourses/R_Packages/R_Package_Development_20180817.html
  op <- options()
  op.econModel <- list(
    # note: In R Studio, need to "restart R" to see changes
    # most useful with
    # options(tryCatchLog.write.error.dump.file = TRUE)
    econModel.tryCatchLog.write.error.dump.folder  = paste0(normalizePath(tempdir(), winslash = "/"), "/", "econModel.tryCatchLog.write.error.dump")
    # in Windows, see this folder
    # writeLines(normalizePath(getOption("econModel.tryCatchLog.write.error.dump.folder"), winslash = "\\"))
  )
  toset <- !(names(op.econModel) %in% names(op))

  if (any(toset)) options(op.econModel[toset])

  if(!dir.exists(getOption("econModel.tryCatchLog.write.error.dump.folder"))) {
    dir.create(getOption("econModel.tryCatchLog.write.error.dump.folder"))
  }
  futile.logger::flog.appender(futile.logger::appender.file(paste0(getOption("econModel.tryCatchLog.write.error.dump.folder"), "/", "econModel.tryCatch.log")))
  futile.logger::flog.threshold(futile.logger::TRACE)   # TRACE, DEBUG, INFO, WARN, ERROR, FATAL

  invisible()
}
