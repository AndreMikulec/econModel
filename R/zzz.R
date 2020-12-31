## Startup functions ------------------------------------

#' .onAttach start message
#'
#' @param libname defunct
#' @param pkgname defunct
#' @return invisible()
#' @rdname onAttach
#' @importFrom tryCatchLog tryCatchLog
.onAttach <- function(libname, pkgname) {
tryCatchLog::tryCatchLog({
  # NOV 2020
  # https://statacumen.com/teach/ShortCourses/R_Packages/R_Package_Development_20180817.html
  start_message <- c(   "\nProviding accesses to social and economic Data from ALFRED and elsewhere\n"
                      , "                       by Andre Mikulec\n"
                      , "\n"
                      , "R package \"econModel\" is *not yet* connected(ing) to a PosgreSQL-like database.\n"
                      , "\n"
                      , "(1) (If not already started), start the  PosgreSQL-like database.\n"
                      , "(2) Run \"dbConnectEM()\" or for help type \"? econModel::dbConnectEM\"\n"
  )
  packageStartupMessage(start_message)
  invisible()
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' .onLoad getOption package settings
#'
#' @param libname defunct
#' @param pkgname defunct
#' @return invisible()
#' @rdname onLoad
#' @examples
#' \dontrun{
#' # See all options
#' options()
#'
#' # See the current temporary working folder
#' getOption("econModel.tryCatchLog.write.error.dump.folder"))
#'
#' # Of my manually self-pre-created folder . . .
#' # Re-assign to my permanent(ish) working folder
#' MyWorkingFolder <- "C:\\Users\\Public\\econModelWork"
#' MyWorkingFolder <- normalizePath(MyWorkingFolder, winslash = "/")
#' options(econModel.tryCatchLog.write.error.dump.folder = MyWorkingFolder)
#' getOption("econModel.tryCatchLog.write.error.dump.folder"))
#' }
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
  if(!"econModel.tryCatchLog.write.error.dump.folder" %in% names(ops)) {
    options(append(ops, list(econModel.tryCatchLog.write.error.dump.folder = paste0(normalizePath(tempdir(), winslash = "/"), "/", "econModel.tryCatchLog.write.error.dump"))))
  }
  rm(ops)
  # # Sometimes useful with post-mortem debugging .rda file creation.
  # # .rda file creation requires:
  # options(tryCatchLog.write.error.dump.file = TRUE)
  if(!dir.exists(getOption("econModel.tryCatchLog.write.error.dump.folder"))) {
    dir.create(getOption("econModel.tryCatchLog.write.error.dump.folder"))
  }
  futile.logger::flog.appender(futile.logger::appender.file(paste0(getOption("econModel.tryCatchLog.write.error.dump.folder"), "/", "econModel.tryCatch.log")))
  futile.logger::flog.threshold(futile.logger::TRACE)   # TRACE, DEBUG, INFO, WARN, ERROR, FATAL

  # temporary or permanent storage

  ops <- options()
  if(!"econmodel_db_storage_name" %in% names(ops)) {
    splitted <- strsplit(normalizePath(tempdir(), winslash = "/"),"/")
    econmodel_db_storage_name <- tolower(splitted[[1]][length(splitted[[1]])])
    rm(splitted)
    ops <- append(ops, list(econmodel_db_storage_name = econmodel_db_storage_name))
    rm(econmodel_db_storage_name)
  }
  options(ops)
  ops <- options()
  if(!"econmodel_db_driver" %in% names(ops)) {
    # currently, I only have an implementation for
    # PostgreSQL (or PostgreSQL-like) databases
    ops <- append(ops, list(econmodel_db_driver = "PostgreSQL"))
  }
  options(ops)
  ops <- options()
  if(!"econmodel_db_user" %in% names(ops)) {
    ops <- append(ops, list(econmodel_db_user = getOption("econmodel_db_storage_name")))
  }
  options(ops)
  ops <- options()
  if(!"econmodel_db_password" %in% names(ops)) {
    ops <- append(ops, list(econmodel_db_password = getOption("econmodel_db_storage_name")))
  }
  options(ops)
  ops <- options()
  if(!"econmodel_db_host" %in% names(ops)) {
    ops <- append(ops, list(econmodel_db_host = "localhost"))
  }
  options(ops)
  ops <- options()
  if(!"econmodel_db_dbname" %in% names(ops)) {
    ops <- append(ops, list(econmodel_db_dbname = getOption("econmodel_db_storage_name")))
  }
  options(ops)
  ops <- options()
  if(!"econmodel_db_port" %in% names(ops)) {
    ops <- append(ops, list(econmodel_db_port = 5432L))
  }
  options(ops)
  ops <- options()
  if(!"econmodel_db_tty" %in% names(ops)) {
    ops <- append(ops, list(econmodel_db_tty = character()))
  }
  options(ops)
  ops <- options()
  if(!"econmodel_db_dboptions" %in% names(ops)) {
    ops <- append(ops, list(econmodel_db_dboptions = character()))
  }
  options(ops)
  ops <- options()
  if(!"econmodel_db_forceISOdate" %in% names(ops)) {
    ops <- append(ops, list(econmodel_db_forceISOdate = TRUE))
  }
  options(ops)

  rm(ops)

  invisible()
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' .onLoad getOption package settings
#'
#' @param libname defunct
#' @param pkgname defunct
#' @return invisible()
#' @rdname onLoad
#' @examples
#' getOption("econModel.name")
#' @importFrom tryCatchLog tryCatchLog
.onUnload <- function(libpath) {
tryCatchLog::tryCatchLog({

  ops <- options()
  ops[["econModel.tryCatchLog.write.error.dump.folder"]] <- NULL
  options(ops)

  invisible()
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



