
#' Get the Data Version of the AAII StockInvestor Pro
#'
#' The American Association of Individual Investors has a software product
#' called StockInvestor Pro. Historically periodically e.g. monthly, but recently
#' daily, new updated corporate data of of 6000+ companies is updated and
#' re-available. The software includes (much) information from the following:
#' income statements, cash flow statements, balance sheets, stock prices,
#' analyst ratings, and company general information.
#' This function reads the file  Setup.dbf file. It is a dBASE file.
#' Note, the .dbf file can be opened interactively (and read)
#' using/through LibreOffice Calc, R package foreign function read.dbf, and
#' other applications that use the file shapefil.h written by Frank Warmerdam.
#'
#' @param From string. Directory of AAII StockInvestor Pro installation directory.  Defaults to "C:/Program Files (x86)/Stock Investor/Professional".
#' @return string. Date of the SIPro update, in days since the UNIX epoch (birthday of UNIX: January 1st, 1970). Returned is the "Current as of date" of StockInvestor Pro.  This is the same data found by doing Help -> About (and then reading the bottom line).
#' @references
#' \cite{foreign/src/shapefil.h
#' \url{https://github.com/cran/foreign/blob/master/src/shapefil.h}
#' }
#' @references
#' \cite{Index of /shapelib/
#' \url{http://download.osgeo.org/shapelib/}
#' }
#' @examples
#' \dontrun{
#' dateAAIISIPro()
#' [1] "18565"
#'
#' zoo::as.Date(as.integer(dateAAIISIPro()))
#' [1] "2020-10-30"
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom foreign read.dbf
#' @export
dateAAIISIPro <- function(From = "C:/Program Files (x86)/Stock Investor/Professional") {
tryCatchLog::tryCatchLog({

  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }

  SetupFile <- suppressWarnings(suppressMessages(foreign::read.dbf(file=paste0(From,"/","Setup.dbf"), as.is = TRUE)))

  if(length(unique(
    as.integer(SetupFile[,"MONTHDATE"])
    , as.integer(SetupFile[,"WEEKDATE"])
    , as.integer(SetupFile[,"SPLITDATE"])
  )) != 1) stop("MONTHDATE != WEEKDATE != SPLITDATE")

  NewRepositoryEntryName <- as.character(as.integer(SetupFile[,"MONTHDATE"]))

  return(NewRepositoryEntryName)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}






#' Copy SIPro .DBF Files to a Target Directory
#'
#' AAII StockInvestor Pro source corporate and price data is stored in .DBF files.
#' Along with the copied .DBF files also the ReadMe.txt and the si.chm help files is copied.
#'
#' @param From string. Directory of AAII StockInvestor Pro installation directory.  Defaults to "C:/Program Files (x86)/Stock Investor/Professional".
#' @param To string. Location of the target directory. Default is in the TEMPDIR.
#' @param CaseChange string. Default is NULL(no change).  Change to target file name to the desired case: NULL(no change), "UpperCase", "LowerCase".
#' @return string. Date of the SIPro update, in days since the UNIX epoch (birthday of UNIX: January 1st, 1970). Returned is the "Current as of date" of StockInvestor Pro.  This is the same data found by doing Help -> About (and then reading the bottom line).
#' @examples
#' \dontrun{
#' copyAAIISIProDBFs(
#'     From = "C:\\Program Files (x86)\\Stock Investor\\Professional",
#'     To   = paste0("C:\\DATA\\AAIISIPRO\\MONTHDATE","\\", dateAAIISIPro()),
#'     CaseChange = "UpperCase"
#' )
#'
#' dir(paste0("C:\\DATA\\AAIISIPRO\\MONTHDATE", "\\", dateAAIISIPro()))
#' }
#' @importFrom tryCatchLog tryCatchLog
copyAAIISIProDBFs <- function(From = "C:\\Program Files (x86)\\Stock Investor\\Professional",
                              To = tempdir(),
                              CaseChange = "UpperCase") {
tryCatchLog::tryCatchLog({

  From <- normalizePath(From, winslash = "/")

  if(!dir.exists(To)) {
    dir.create(To)
  }
  # normalizePath, first "Checks" to make sure that the directory exists
  To   <- normalizePath(To, winslash = "/")

  SubDirs <- c("","/Dbfs","/User","/Static","/Temp","/Datadict")

  for(SubDir in SubDirs) {

    # it DOES not overwrite what is ALREADY there
    R.utils__copyDirectoryByPattern(from = paste0(From, SubDir),
                                    pattern = "(*[.]dbf$|[.]*DBF$|*[.]chm$|ReadMe[.]txt)",
                                    to = To, CaseChange = CaseChange
    )

  }

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}




#' FileName With Its Extension
#'
#' Get the FileName with its extension.
#'
#' @param SubPathFileExt String. Path to the file including FileName and its (after the dot ".") extension.
#' @param Separ String. Regular expression separating the FileName with its extension from the rest of the path.
#' @param PathReq Logical. Send SubPathFileExt through normalizePath.
#' @param ... Dots passed.
#' @returns String.
#' @examples
#' \dontrun{
#' fileAndExt(c(
#'   "C:\\Program Files (x86)\\Stock Investor\\Professional\\setup.dbf",
#'   "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_isq.dbf"
#' ))
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @export
fileAndExt <- function(SubPathFileExt, Separ = "/", PathReq = TRUE, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  if(missing(SubPathFileExt)) {
    stop("Parameter \"SubPathFileExt\" is required.")
  }
  if(PathReq) {
    SubPathFileExt <- normalizePath(SubPathFileExt, winslash = "/")
  }

  OtherReturns <- raw()
  if(1L < NROW(SubPathFileExt)) {
    OtherReturns <- sapply(SubPathFileExt[seq_along(SubPathFileExt)[-1L]], function(SubPathFileExt) {
      DescTools::DoCall("fileAndExt", c(list(), list(SubPathFileExt = SubPathFileExt), list(Separ = Separ), list(PathReq = PathReq), Dots[Names(Dots) %in% "fileAndExt"]))
    }, USE.NAMES = FALSE)
  }

  Return <- last(strsplit(SubPathFileExt[1], Separ)[[1]])

  return(c(Return, OtherReturns))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' FilePartName
#'
#' Get the FilePartName.
#'
#' @param SubPathFileExt String. Path to the file including FileName and its (after the dot ".") extension.
#' @param Separ String. Regular expression separating the FileName from its extension.
#' @param PathReq Logical. Send SubPathFileExt through normalizePath.
#' @param ... Dots passed.
#' @returns String.
#' @examples
#' \dontrun{
#' filePartName(c(
#'   "C:\\Program Files (x86)\\Stock Investor\\Professional\\setup.dbf",
#'   "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_isq.dbf"
#' ), Position = "first")
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @export
filePartName <- function(SubPathFileExt, Separ = "[.]", PathReq = TRUE, Position, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  if(missing(SubPathFileExt)) {
    stop("Parameter \"SubPathFileExt\" is required.")
  }

  if(missing(Position)) {
    stop("Parameter \"Position\" is required.")
  }

  if(PathReq) {
    SubPathFileExt <- normalizePath(SubPathFileExt, winslash = "/")
  }

  OtherReturns <- raw()
  if(1L < NROW(SubPathFileExt)) {
    OtherReturns <- sapply(SubPathFileExt[seq_along(SubPathFileExt)[-1L]], function(SubPathFileExt) {
      DescTools::DoCall("filePartName", c(list(), list(SubPathFileExt = SubPathFileExt), list(Separ = Separ), list(PathReq = PathReq), list(Position = Position), Dots[Names(Dots) %in% "filePartName"]))
    }, USE.NAMES = FALSE)
  }

  FileAndExtReturn <- DescTools::DoCall("fileAndExt", c(list(), list(SubPathFileExt = SubPathFileExt[1]), Dots[Names(Dots) %in% "fileAndExt"]))
  SplitReturn <- strsplit(FileAndExtReturn, Separ)[[1]]

  if(Position == "first" || Position == 1) {
    Return <- first(SplitReturn)
  }
  if(Position == "last") {
    Return <- last(SplitReturn)
  }
  if(is.numeric(Position)) {
    Return <- SplitReturn[Position]
  }

  return(c(Return, OtherReturns))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}





#' FileName Without Its Extension
#'
#' Get the FileName without its extension.
#'
#' @param SubPathFileExt String. Path to the file including FileName and its (after the dot ".") extension.
#' @param Separ String. Regular expression separating the FileName from its extension.
#' @param PathReq Logical. Send SubPathFileExt through normalizePath.
#' @param ... Dots passed.
#' @returns String.
#' @examples
#' \dontrun{
#' fileName(c(
#'   "C:\\Program Files (x86)\\Stock Investor\\Professional\\setup.dbf",
#'   "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_isq.dbf"
#' ))
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @export
fileName <- function(SubPathFileExt, Separ = "[.]", PathReq = TRUE, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  if(missing(SubPathFileExt)) {
    stop("Parameter \"SubPathFileExt\" is required.")
  }

  if(PathReq) {
    SubPathFileExt <- normalizePath(SubPathFileExt, winslash = "/")
  }

  OtherReturns <- raw()
  if(1L < NROW(SubPathFileExt)) {
    OtherReturns <- sapply(SubPathFileExt[seq_along(SubPathFileExt)[-1L]], function(SubPathFileExt) {
      DescTools::DoCall("fileName", c(list(), list(SubPathFileExt = SubPathFileExt), list(Separ = Separ), list(PathReq = PathReq), Dots[Names(Dots) %in% "fileName"]))
    }, USE.NAMES = FALSE)
  }

  Return <- strsplit(DescTools::DoCall("filePartName", c(list(), list(SubPathFileExt = SubPathFileExt[1]), list(Position = "first"), Dots[Names(Dots) %in% "filePartName"])), Separ)[[1]]

  return(c(Return, OtherReturns))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' File Extension
#'
#' Get the File Extension.
#'
#' @param SubPathFileExt String. Path to the file including FileName and its (after the dot ".") extension.
#' @param Separ String. Regular expression separating the File Extension from its FileName.
#' @param PathReq Logical. Send SubPathFileExt through normalizePath.
#' @param ... Dots passed.
#' @returns String.
#' @examples
#' \dontrun{
#' fileExt(c(
#'   "C:\\Program Files (x86)\\Stock Investor\\Professional\\setup.dbf",
#'   "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_isq.dbf"
#' ))
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @export
fileExt <- function(SubPathFileExt, PathReq = TRUE, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  if(missing(SubPathFileExt)) {
    stop("Parameter \"SubPathFileExt\" is required.")
  }

  if(PathReq) {
    SubPathFileExt <- normalizePath(SubPathFileExt, winslash = "/")
  }

  OtherReturns <- raw()
  if(1L < NROW(SubPathFileExt)) {
    OtherReturns <- sapply(SubPathFileExt[seq_along(SubPathFileExt)[-1L]], function(SubPathFileExt) {
      DescTools::DoCall("fileExt", c(list(), list(SubPathFileExt = SubPathFileExt), list(PathReq = PathReq), Dots[Names(Dots) %in% "fileExt"]))
    }, USE.NAMES = FALSE)
  }

  Return <- DescTools::DoCall("filePartName", c(list(), list(SubPathFileExt = SubPathFileExt[1]), list(Position = "last"), Dots[Names(Dots) %in% "filePartName"]))

  return(c(Return, OtherReturns))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' FileName FilePositionName Without Its Extension
#'
#' Get the FileName FilePositionName without its extension.
#'
#' @param SubPathFileExt String. Path to the file including FileName and its (after the dot ".") extension.
#' @param Separ String. Regular expression separating the FilePostionName from its FileName.
#' @param PathReq Logical. Send SubPathFileExt through normalizePath.
#' @param Position String or Number. Required. Can be "first", 1, "last", or a number of the position.
#' @param ... Dots passed.
#' @returns String.
#' @examples
#' \dontrun{
#' filePositionName(c(
#'   "C:\\Program Files (x86)\\Stock Investor\\Professional\\setup.dbf",
#'   "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_isq.dbf"
#' ), Position = "first")
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @export
filePositionName <- function(SubPathFileExt, Separ = "_", PathReq = TRUE, Position, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  if(missing(SubPathFileExt)) {
    stop("Parameter \"SubPathFileExt\" is required.")
  }

  if(missing(Position)) {
    stop("Parameter \"Position\" is required.")
  }

  if(PathReq) {
    SubPathFileExt <- normalizePath(SubPathFileExt, winslash = "/")
  }

  OtherReturns <- raw()
  if(1L < NROW(SubPathFileExt)) {
    OtherReturns <- sapply(SubPathFileExt[seq_along(SubPathFileExt)[-1L]], function(SubPathFileExt) {
      DescTools::DoCall("filePositionName", c(list(), list(SubPathFileExt = SubPathFileExt), list(Separ = Separ), list(PathReq = PathReq), list(Position = Position), Dots[Names(Dots) %in% "filePositionName"]))
    }, USE.NAMES = FALSE)
  }

  FileNameReturn <- DescTools::DoCall("fileName", c(list(), list(SubPathFileExt = SubPathFileExt[1]), Dots[Names(Dots) %in% "fileName"]))
  SplitReturn    <- strsplit(FileNameReturn, Separ)[[1]]

  if(Position == "first" || Position == 1) {
    Return <- first(SplitReturn)
  }
  if(Position == "last") {
    Return <- last(SplitReturn)
  }
  if(is.numeric(Position)) {
    Return <- SplitReturn[Position]
  }

  return(c(Return, OtherReturns))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' FileName FilePostFixName Without Its Extension
#'
#' Get the FileName FilePostFixName without its extension.
#'
#' @param SubPathFileExt String. Path to the file including FileName and its (after the dot ".") extension.
#' @param Separ String. Regular expression separating the FilePostFixName from its FileName.
#' @param PathReq Logical. Send SubPathFileExt through normalizePath.
#' @param ... Dots passed.
#' @returns String.
#' @examples
#' \dontrun{
#' filePostFixName(c(
#'   "C:\\Program Files (x86)\\Stock Investor\\Professional\\setup.dbf",
#'   "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_isq.dbf"
#' ))
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @export
filePostFixName <- function(SubPathFileExt, Separ = "_", PathReq = TRUE, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  if(missing(SubPathFileExt)) {
    stop("Parameter \"SubPathFileExt\" is required.")
  }

  if(PathReq) {
    SubPathFileExt <- normalizePath(SubPathFileExt, winslash = "/")
  }

  # does accept a vector
  Return <- DescTools::DoCall("filePositionName", c(list(), list(SubPathFileExt = SubPathFileExt), list(Separ = Separ), list(PathReq = PathReq), list(Position = "last"), Dots[Names(Dots) %in% "filePositionName"]))

  return(Return)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' FileName FilePreFixName Without Its Extension
#'
#' Get the FileName FilePreFixName without its extension.
#'
#' @param SubPathFileExt String. Path to the file including FileName and its (after the dot ".") extension.
#' @param Separ String. Regular expression separating the FilePreFixName from its FileName.
#' @param PathReq Logical. Send SubPathFileExt through normalizePath.
#' @param ... Dots passed.
#' @returns String.
#' @examples
#' \dontrun{
#' filePreFixName(c(
#'   "C:\\Program Files (x86)\\Stock Investor\\Professional\\setup.dbf",
#'   "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_isq.dbf"
#' ))
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @export
filePreFixName <- function(SubPathFileExt, Separ = "_", PathReq = TRUE, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  if(missing(SubPathFileExt)) {
    stop("Parameter \"SubPathFileExt\" is required.")
  }

  if(PathReq) {
    SubPathFileExt <- normalizePath(SubPathFileExt, winslash = "/")
  }

  # does accept a vector
  Return <- DescTools::DoCall("filePositionName", c(list(), list(SubPathFileExt = SubPathFileExt), list(Separ = Separ), list(PathReq = PathReq), list(Position = "first"), Dots[Names(Dots) %in% "filePositionName"]))

  return(Return)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' View Object
#'
#' From the "env" environment.  Calls View() upon each R object.
#'
#' @param env Target environment or (list of environments) to view R objects. Required.
#' @param ... Dots passed.
#' @returns None.
#' @examples
#' \dontrun{
#' e <- new.env()
#' assign("iris", iris, envir = e)
#' assign("airquality", airquality, envir = e)
#' viewObject(e)
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @export
viewObject <- function(env, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  # object of type 'environment' is not subsettable
  if(missing(env)) {
    stop("Parameter \"env\" is required.")
  }

  OtherReturns <- raw()
  if(is.list(env) && 1L < NROW(env)) {
    OtherReturns <- sapply(env[seq_along(env)[-1L]], function(env) {
      DescTools::DoCall("viewObject", c(list(), list(env = env), Dots[Names(Dots) %in% "viewObject"]))
    }, USE.NAMES = FALSE)
  }


  # object of type 'environment' is not subsettable
  if(!is.environment(env)) {
    stop("Parameter \"env\" must be of class \"environment\".")
  }

  # object of type 'environment' is not subsettable
  with(env, {

    # do not pollute the environment with "User" variables
    if(1L < NROW(ls())) {
                       sapply(ls()[seq_along(ls())[-1L]], function(ObjectName) {
        # DescTools::DoCall("View", c(list(), list(ObjectName)))
        eval(parse(text=paste0("View(", ObjectName,")")))
      }, USE.NAMES = FALSE)
    }

    eval(parse(text=paste0("View(",ls()[1],")")))

  })

  return(invisible(character()))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Load File
#'
#' Creates a data.frame from each file.  Loads those data.frames into the "env" environment.  Calls View() upon each data.frame (so the user may see the data).
#'
#' @param SubPathFileExt Location of the file. Required.
#' @param HowFileName String. Function name to determine which part (or all of the FileName) to be the new R object name.  Expected values are "fileName", "filePostFixName", or "filePreFixName".
#' @param env Target environment to load data.frames. Default is the global environment (.GlobalEnv)
#' @param ... Dots passed.
#' @returns String. Name of the data.frame loaded. data.frames are loaded into the environment "env".
#' @examples
#' \dontrun{
#' e <- new.env()
#' loadFile(c(
#'   "C:\\Program Files (x86)\\Stock Investor\\Professional\\setup.dbf",
#'   "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_isq.dbf"
#' ), env = e)
#'
#' eFST <- new.env()
#' DIR <- "C:\\DATA\\AAIISIPRO\\MONTHDATE\\18565"
#' FSTFiles <- paste0( DIR, "\\" , dir(DIR, pattern = glob2rx("*.FST")))
#' loadFile(FSTFiles, env = eFST)
#' str(as.list(eFST))
#'
#' eDBF <- new.env()
#' DIR <- "C:\\DATA\\AAIISIPRO\\MONTHDATE\\18565"
#' DBFFiles <- paste0( DIR, "\\" , dir(DIR, pattern = glob2rx("*.DBF")))
#' loadFile(DBFFiles, env = eDBF)
#' str(as.list(eDBF))
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom foreign read.dbf
#' @importFrom fst read.fst
#' @importFrom DescTools DoCall
#' @export
loadFile <- function(SubPathFileExt, HowFileName = "fileName", env, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  if(missing(SubPathFileExt)) {
    stop("Parameter \"SubPathFileExt\" is required.")
  }

  if( 1L < NROW(HowFileName) || !HowFileName %in% c("fileName", "filePostFixName", "filePreFixName")) {
    stop("Parameter \"HowFileName\" must be one of \"fileName\", \"filePostFixName\", and \"filePreFixName\".")
  }

  if(missing(env)) {
    env <- .GlobalEnv
  }

  OtherReturns <- raw()
  if(1L < NROW(SubPathFileExt)) {
    OtherReturns <- sapply(SubPathFileExt[seq_along(SubPathFileExt)[-1L]], function(SubPathFileExt) {
      DescTools::DoCall("loadFile", c(list(), list(SubPathFileExt = SubPathFileExt), list(env = env), Dots[Names(Dots) %in% "loadFile"]))
    }, USE.NAMES = FALSE)
  }

  ReturnFileExt  <- DescTools::DoCall("fileExt", c(list(), list(SubPathFileExt = SubPathFileExt[1]), Dots[Names(Dots) %in% "fileExt"]))

  if(toupper(ReturnFileExt) == "DBF") {
    cat("Loading file: ", SubPathFileExt[1], ". . . . ")
    ReturnDataFrame <- suppressWarnings(suppressMessages(foreign::read.dbf(SubPathFileExt[1], as.is = TRUE)))
    cat("Done.\n")
  }

  if(toupper(ReturnFileExt) == "FST") {
    cat("Loading file: ", SubPathFileExt[1], ". . . . ")
    ReturnDataFrame <- fst::read.fst(SubPathFileExt[1])
    cat("Done.\n")
  }

  ReturnHowFileName <- DescTools::DoCall(HowFileName, c(list(), list(SubPathFileExt = SubPathFileExt[1]), Dots[Names(Dots) %in% HowFileName]))
  assign(ReturnHowFileName, value = ReturnDataFrame, envir = env)

  return(c(ReturnHowFileName, OtherReturns))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}





#' Load and View Files
#'
#' Creates a data.frame from each  file.  Loads those data.frames into the "env" environment.  Calls View() upon each data.frame (so the user may see the data).
#'
#' @param SubPathFileExt Location of the file.
#' @param HowFileName String. Function name to determine which part (or all of the FileName) to be the new R object name.  Expected values are "fileName", "filePostFixName", or "filePreFixName".
#' @param env Target environment to load data.frames. Default is the global environment (.GlobalEnv)
#' @returns String. Name of the data.frame loaded. data.frames are loaded into the environment.  Attempted to be started is View() upon each data.frame.
#' @examples
#' \dontrun{
#' e <- new.env()
#' loadAndView(c(
#'   "C:\\Program Files (x86)\\Stock Investor\\Professional\\setup.dbf",
#'   "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_isq.dbf"
#' ), env = e)
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @export
loadAndView <- function(SubPathFileExt, HowFileName = "fileName", env, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  if(missing(SubPathFileExt)) {
    stop("Parameter \"SubPathFileExt\" is required.")
  }

  if( 1L < NROW(HowFileName) || !HowFileName %in% c("fileName", "filePostFixName", "filePreFixName")) {
    stop("Parameter \"HowFileName\" must be one of \"fileName\", \"filePostFixName\", and \"filePreFixName\".")
  }

  if(missing(env)) {
    env <- .GlobalEnv
  }

  OtherReturns <- raw()
  if(1L < NROW(SubPathFileExt)) {
    OtherReturns <- sapply(SubPathFileExt[seq_along(SubPathFileExt)[-1L]], function(SubPathFileExt) {
      DescTools::DoCall("loadAndView", c(list(), list(SubPathFileExt = SubPathFileExt), list(HowFileName = HowFileName), list(env = env), Dots[Names(Dots) %in% "loadAndView"]))
    }, USE.NAMES = FALSE)
  }
  # just now placed in "env"
  ReturnLoadFile <- DescTools::DoCall("loadFile", c(list(), list(SubPathFileExt = SubPathFileExt[1]), list(HowFileName = HowFileName), list(env =  env), Dots[Names(Dots) %in% "loadFile"]))

  e <- new.env()
  assign(ReturnLoadFile , get(ReturnLoadFile, envir = env), env = e)
  # just view the last one that was just previously now placed into the environment "env"
  DescTools::DoCall("viewObject", c(list(), list(env = e), Dots[Names(Dots) %in% "viewObject"]))
  rm(e)

  return(c(ReturnLoadFile, OtherReturns))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}




#' Load and View AAII StockInvestor Pro FoxPro/DBase (DBF) Files
#'
#' Creates a data.frame from each  file.  Loads those data.frames into the .Global environment.  Calls View() upon each data.frame (so the user may see the data).
#'
#' @param Collection String.  Some of "Base", "Prices", "Sheets", or "Dictionary". Required.
#' @param Source String.  One of "Install" or "Repository".
#' @param SubDir String.  Single directory containing the "DBF" files. If "Source" = "Repository", then Required. Otherwise, ignored.
#' @param Ext String. Default is "DBF". This is always the case when "Source = "Install": so, this parameters is ignored.  Alternatively, this parameter can be "FST" when "Source = "Repository"".
#' @returns String. Name of the data.frame loaded. data.frames are loaded into the environment.  Attempted to be started is View() upon each data.frame.
#' @examples
#' \dontrun{
#' viewSIPRO("Base")
#' viewSIPRO("Base", Source = "Repository", SubDir = "C:\\DATA\\AAIISIPRO\\MONTHDATE\\18565")
#' viewSIPRO("Base", Source = "Repository", SubDir = "C:\\DATA\\AAIISIPRO\\MONTHDATE\\18565", Ext = "FST")
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @export
viewSIPRO <- function(Collection, Source = "Install", SubDir, Ext = "DBF", ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  if(missing(Collection)) {
    stop("Parameter \"Collection\" is required.")
  }

  if( !Collection %in% c("Base", "Prices", "Sheets", "Dictionary")) {
    stop("Parameter \"Collection\" must be some of \"Base\", \"Prices\", \"Sheets\", or \"Dictionary\".")
  }

  if(!missing(SubDir)) {
    SubDir <- normalizePath(SubDir, winslash = "/")
  }

  if( 1L < NROW(Source) || !Source %in% c("Install", "Repository")) {
    stop("Parameter \"Source\" must be one of \"Install\" or \"Repository\".")
  }

  if(Source == "Repository" && missing(SubDir)) {
    stop("Parameter \"SubDir\" required when parameter \"Source\" = \"Repository\".")
  }

  if(!Ext %in% c("DBF","FST")) {
    stop("Parameter \"Ext\" most be one of  \"DBF\" or \"FST\".")
  }

  if(Ext %in% "FST" && Source == "Install") {
    stop("Parameter \"FST\" files will not be \"looked for\" in the \"Install\" directory.")
  }

  OtherReturns <- raw()
  if(1L < NROW(Collection)) {
    OtherReturns <- sapply(Collection[seq_along(Collection)[-1L]], function(Collection) {
      DescTools::DoCall("viewSIPRO", c(list(), list(Collection = Collection), list(Source = Source), list(SubDir = SubDir), Dots[Names(Dots) %in% "viewSIPRO"]))
    }, USE.NAMES = FALSE)
  }

  FullCollection <- list()

  # Install area

  FullCollection[["Dictionary"]] <- c(
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Datadict\\filemast.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Datadict\\datadict.dbf"
  )

  FullCollection[["Base"]] <- c(
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\setup.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_ci.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\SI_EXCHG.DBF",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_sp.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_ptyp.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_date.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_utyp.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_trbcs.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\SI_MGDSC.DBF"
  )

  # Needs "Base"
  FullCollection[["Prices"]] <- c(
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Dbfs\\si_psd.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Dbfs\\si_psdc.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Dbfs\\si_psdd.dbf"
  )

  # Needs "Base"
  FullCollection[["Sheets"]] <- c(
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_isq.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_bsq.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_cfq.dbf"
  )


  SelectedFromFullCollection <- unlist(FullCollection[match(Collection, Names(FullCollection))], use.names = FALSE)

  if(Source == "Repository") {

    # Repository Area

    Coordinates <- regexpr("\\w+[.]\\w+$", SelectedFromFullCollection, perl = TRUE)
    SelectedFromFullCollection <-  toupper(substr(SelectedFromFullCollection, Coordinates, Coordinates + attr(Coordinates, "match.length") - 1L))

    SelectedFromFullCollection <- paste0(SubDir, "/", SelectedFromFullCollection)
    if(Ext == "DBF") {
      SelectedFromFullCollection <- SelectedFromFullCollection
    }

    if(Ext == "FST") {
      SelectedFromFullCollection <- sub("DBF$", "FST", SelectedFromFullCollection)
    }

  }

  FileExistsConclusion <- file.exists(SelectedFromFullCollection)
  if(!any(FileExistsConclusion)) {
    stop(paste0("Missing requested files: ", paste0(SelectedFromFullCollection[!FileExistsConclusion], collapse = " and ")  , " . . . . Aborting."))
  }

  ReturnLoadAndView <- DescTools::DoCall("loadAndView", c(list(), list(SubPathFileExt = SelectedFromFullCollection), list(HowFileName = "filePostFixName"), list(env = .GlobalEnv), Dots[Names(Dots) %in% "loadAndView"]))

  return(ReturnLoadAndView)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}





#' convert DBF files and data.frames to data.tables and FST files.
#'
#' Removed useless columns.  Remove duplicated data.  Change column datatypes.
#'
#' @param From Directory containing the Files (xor R object list of data.frames - NOT-IMPLEMENTED).
#' @param FromFiles if From is a directory, then a vector of "DBF" files of interest.
#' @param To if From is a directory, then the new directory location (xor R object list of data.frames - NOT-IMPLEMENTED).
#' @param PrependColFile DBF file that has the  Unique identifier column name.  This is the first column.
#' @param RemoveCols Vector of regular expressions(PERL = T) of columns to remove.
#' @param RemoveDupsColFileExceptions Files to non-remove duplicate column values.  See the next parameter. RemoveDupsColValues.
#' @param RemoveDupsColValues Column name to have its duplicates (and an corresponding non-duplicates) removed.
#' @param ChangeType list of named vectors, with the name of the vector to be the output datatype, and the values of the vectors to be regular expressions identifying the columns to be converted.  Remaining columns not yet converted are converted to numeric.
#' @return If "From" is a directory, then new files are placed on disk. Alternately, if "From" is an R list, then return a list of modified data.tables.
#' @examples
#' \dontrun{
#' formatDBFs()
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom foreign read.dbf
#' @importFrom DataCombine MoveFront
#' @importFrom data.table as.data.table
#' @export
formatDBFs <- function(From = paste0("C:/DATA/AAIISIPRO/MONTHDATE","/", 18565),
                                     # Base
                       FromFiles = c("SETUP.DBF",
                                     "SI_CI.DBF" , "SI_EXCHG.DBF", "SI_SP.DBF", "SI_PTYP.DBF",
                                     "SI_DATE.DBF", "SI_UTYP.DBF", "SI_TRBCS.DBF", "SI_MGDSC.DBF",
                                     # Prices
                                     "SI_PSD.DBF", "SI_PSDC.DBF", "SI_PSDD.DBF",
                                     # Sheets
                                     "SI_ISQ.DBF", "SI_BSQ.DBF" , "SI_CFQ.DBF"
                                     ),
                       To = From,
                       PrependColFile = "SETUP.DBF",
                       PrefixCols = c(SI_CI = "LASTMOD",
                                      SI_MGDSC = "MG_CODE", SI_MGDSC = "MG_DESC",
                                      SI_TRBCS = "MG_CODE", SI_TRBCS = "MG_DESC",
                                      SI_PTYP  = "TYPE_CODE", SI_PTYP = "TYPE_DESCR",
                                      SI_UTYP  = "TYPE_CODE", SI_UTYP = "TYPE_DESCR"
                                    ),                                 # only one that has different dates
                       RemoveCols = c("^X.*$", "X_NullFlags", "REPNO", "(?<!CI_)LASTMOD", "UPDATED"),
                       # because the "UTYP" codes (re-statements) are in here
                       RemoveDupsColFileExceptions = c(""), # SI_DATE.DBF only has the most recent 'statement' (so no DUPS)
                       RemoveDupsColValues = c("COMPANY_ID"),
                                                # Dates seem to be already Dates (NOTHING TO DO)
                       ChangeType = list(Date = c("^.*DATE$", "^PRICED.*$", "^.*DT$", "^.*LASTMOD$", "^PEREND_.*$"),
                                                   # Logicals seem to be already Logicals (NOTHING TO DO)
                                         logical = c("^ADR$", "^OPTIONABLE$", "^DRP_AVAIL$", "^UPDATED$"),
                                         integer = c("^EMPLOYEES$", "^PERLEN_.*$", "^SHRINSTN$"),
                                                     # Characters seem to be already Characters (NOTHING TO DO)
                                         character = c("^COMPANY_ID$", "^COMPANY$", "^TICKER$", "^EXCHANGE$", "^STREET$", "^CITY$", "^STATE$", "^ZIP$",
                                                      "^COUNTRY$", "^PHONE$", "^WEB_ADDR$", "^BUSINESS$", "^ANALYST_FN$", "IND_2_DIG", "^IND_3_DIG$", "^SIC$", "^SP$", "^DOW$",
                                                      "^EXCHG_CODE$", "^EXCHG_DESC$", "^.*MG_CODE$", "^.*MG_DESC$",
                                                      "^PERTYP_.*$", "^UPDTYP_.*$",
                                                      "^SP_CODE$", "^SP_DESC$", "^.*TYPE_CODE$", "^.*TYPE_DESCR$", "^TYPE_SHORT$")

                                        )
                       ) {
tryCatchLog::tryCatchLog({

  # PrependColFile garanteed to be first
  if(length(PrependColFile)) {
    if(PrependColFile %in% FromFiles) {
       # garanteed SETUP.DBF moves to front
      FromFiles <- c(PrependColFile, FromFiles[!FromFiles %in% PrependColFile])
    }
  }
  RetList <- list()
  if(length(FromFiles)) {
    for(FromFile in FromFiles) {

      PathandFile <- paste0(From, "/", FromFile)
      cat(paste0("Attempting to read: ", PathandFile,"\n"))
      ReadFile <- suppressWarnings(suppressMessages(foreign::read.dbf(file =PathandFile, as.is = TRUE)))
      cat(paste0("Succesfully read: ", PathandFile,"\n"))

      FromFileRoot <- strsplit(FromFile, "[.]")[[1]][1]
      # rename columns
      if(length(PrefixCols) && FromFileRoot %in% Names(PrefixCols)) {
        #  PrefixCols = c(SI_CI = "LASTMOD")
        mapply(function(ColTable, ColName) {
          if(FromFileRoot == ColTable) {
            NewPreFix <- last(strsplit(ColTable, "_")[[1]])
            cat(paste0("Renaming: ", PathandFile, " column: ", ColName, " name to ", paste0(NewPreFix, "_", colnames(ReadFile)[colnames(ReadFile) == ColName]), ". . . . "))
            colnames(ReadFile)[colnames(ReadFile) == ColName] <<- paste0(NewPreFix, "_", colnames(ReadFile)[colnames(ReadFile) == ColName])
            cat("Done.\n")
          }
        },Names(PrefixCols), PrefixCols, SIMPLIFY = FALSE)
      }

      # change datatypes
      AllFilesMayHaveBeenConverted <- list()
      AllColumnsMayHaveBeenConverted <- character()
      mapply(function(Type, Regexes) {
         ColNamesToConvert <- colnames(ReadFile)[grepl(paste0(Regexes, collapse = "|"), colnames(ReadFile), perl = TRUE)]
         if(length(ColNamesToConvert)) {
           if(Type == "Date") {
             TypeConverter  <- "zoo::as.Date"
             NotTypeChecker <-  "!inherits(ReadFile[[ColName]], \"Date\")"
           } else {
             TypeConverter  <- paste0("as.", Type)
             NotTypeChecker <- paste0("!is.", Type,"(ReadFile[[ColName]])")
           }
           for(ColName in ColNamesToConvert) {
             # save some CPU cycles
             if(eval(parse(text=NotTypeChecker))) {
               cat(paste0("Converting: ", PathandFile, " column: ", ColName, " to ", Type, ". . . . "))
               ops <- options()
               options(warn = 2)
               ReadFile[[ColName]] <<- eval(parse(text = paste0(TypeConverter, "(ReadFile[[ColName]])")))
               options(ops)
               cat("Done.\n")
             }
           }
           AllColumnsMayHaveBeenConverted <<- c(AllColumnsMayHaveBeenConverted, ColNamesToConvert)
           invisible()
         }
      }, Names(ChangeType), ChangeType, SIMPLIFY = FALSE)

      # remove columns (must come NOW-after because I have to rename(save) first-ABOVE)
      ReadFile <- ReadFile[, !grepl(paste0(RemoveCols, collapse = "|"), colnames(ReadFile), perl = TRUE), drop = FALSE]

      # convert all OTHER columns (many many cols of type character) to type "numeric" (double)
      lapply(setdiff(colnames(ReadFile), AllColumnsMayHaveBeenConverted), function(ColName) {
        # many derived stats are already numeric
        if(!is.numeric(ReadFile[[ColName]])) {
          cat(paste0("Converting: ", PathandFile, " column: ", ColName, " to ", "numeric", ". . . . "))
          ops <- options()
          options(warn = 2)
          ReadFile[[ColName]] <<- as.numeric(ReadFile[[ColName]])
          options(ops)
          cat("Done.\n")
        }
        invisible()
      })

      # remove duplicates
      if(!FromFile %in% RemoveDupsColFileExceptions) {
        if(length(RemoveDupsColValues) && any(RemoveDupsColValues %in% colnames(ReadFile))) {
           for(RemoveDupsColValue in RemoveDupsColValues) {
             ReadFile <- ReadFile[!(duplicated(unlist(ReadFile[, RemoveDupsColValue])) | duplicated(unlist(ReadFile[, RemoveDupsColValue]), fromLast=TRUE)), , drop = FALSE]
           }
        }
      }
      # determine
      # PrependColFile garanteed to be first (SEE above)
      if(length(PrependColFile) && PrependColFile == FromFile) {
        PrependColId = colnames(ReadFile)[1]
        PrependColValue <- unlist(ReadFile[, PrependColId])
      }
      # actually do
      if(exists("PrependColValue")) {
        ReadFile[[PrependColId]] <- PrependColValue
        ReadFile <- DataCombine::MoveFront(ReadFile, PrependColId)
      }
      # ReadFile <- data.table::as.data.table(ReadFile)
      # means a file path
      PathandFile <- paste0(To, "/", FromFileRoot, ".FST")
      if(is.character(From)) {
        cat(paste0("Attempting to write: ", PathandFile,"\n"))
        fst::write.fst(ReadFile, path = PathandFile, compress = 100L)
        cat(paste0("Successfully wrote: ", PathandFile,"\n"))
      } else {
      # assume a list
      ListItem <- list(ReadFile)
      Names(ListItem)[1] <- FromFileRoot
      RetList <- c(list(),RetList, ListItem)
      }
    cat("\n")
    }
    if(is.character(From)) {
      return(invisible())
    } else {
      return(RetList)
    }
  }
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


#' Results column names are always in UPPERCASE.
#'
#' Thin wrapper over R CRAN package DBI function dbGetQuery
#'
#' @param conn A DBIConnection object, as returned by dbConnect().
#' @param statement	a character string containing SQL.
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @param ...	Other parameters passed on to methods.
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbGetQuery
#' @export
dbGetQueryEM <- function(conn, Statement, display = TRUE, exec = TRUE, ...) {
tryCatchLog::tryCatchLog({

  #correct for TZ
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  on.exit({Sys.setenv(TZ=oldtz)})

  Dots <- list(...)

  tmp.query <- Statement
  ## Display the query
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }

  ## Execute the query and return TRUE
  if (exec) {
    Results <- try({DBI::dbGetQuery(conn, statement = tmp.query, ...)})
    colnames(Results) <- toupper(colnames(Results))
    return(Results)
  }

  invisible()

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}






#' Results column names are always in UPPERCASE.
#'
#' Thin wrapper over R CRAN package caroline function dbWriteTable2
#'
#' @param conn A DBIConnection object, as returned by dbConnect().


#' Insert a new table into the PostgreSQL database or append data.
#'
#' Thin wrapper over R CRAN package caroline function dbWriteTable2
#'
#' @param conn A DBIConnection object, as returned by dbConnect().
#' @param DfName  String.  Default is substitute(Df). The name of the table to which the data frame is to be loaded.
#' @param Df, data.frame. Required. To be loaded to the database.
#' @param FillNull Logical.  Default is TRUE. Should new db present fields be added to the data.frame before it is loaded?
#' @param AddID Logical. Default is FALSE.  Should a new column should be added for the database id?
#' @param RowNames Logical. Default is FALSE.  Should the row names be loaded as a separate column? (unlike the original dbWriteTable, default is FALSE).
#' @param PgUpdateSeq Logical. Default is FALSE. Should the table primary key's sequence be updated to the highest id value +1? (Postgres specific)
#' @param lowerDfName Logical. Default is TRUE. Make the target database table name to be in lowercase.
#' @param lowerColNames Logical. Default is TRUE. Make the target database table column names to be in lowercase.
#' @param replaceDotUsingUnderscore Logical. Default is TRUE. Make the target database table column names internal "dots" be converted to underscores(_).
#' @param ... Dots. Other parameters passed to R CRAN package DBI dbWriteTable.
#' @examples
#' \dontrun{
#' mtcars2 <- mtcars
#' mtcars2[["model"]] <- rownames(mtcars2)
#' mtcars2 <- DataCombine::MoveFront(mtcars2, Var = "model")
#' mtcars2[["vs"]] <- as.logical(mtcars2[["vs"]])
#' mtcars2[["gear"]] <- as.integer(mtcars2[["gear"]])
#' mtcars2[["carb"]] <- zoo::as.Date(mtcars2[["carb"]])
#' rownames(mtcars2) <- NULL
#' #
#' # Creates the table (with zero rows).
#' # Appends data (with the Df having the same columns that the server).
#' mtcars2s <- mtcars2[1:5,]
#' dbWriteTableEM(get("connEM"), Df = mtcars2s)
#'
#' # Appends data (with the Df having less columns that the server database).
#' # Those server columns, that are not found in the Df, are added to the Df.
#' mtcars2lDf <- mtcars2[6:10, "model", drop = F]
#' dbWriteTableEM(get("connEM"), Df = mtcars2lDf)
#'
#' # Appends data (with the server database having less columns that the Df).
#' # Those Df columns, that are not found in the server, are added to the sever.
#' mtcars2lSv <- {DfNew <- mtcars2[11:15, c("model","vs", "am", "gear", "carb")]
#'                colnames(DfNew) <- paste0(colnames(DfNew),"_new")
#'                DfNew[["model"]] <- DfNew[["model_new"]]
#'                DfNew <- DataCombine::MoveFront(DfNew, Var = "model")
#'                DfNew
#'               }; rm(DfNew)
#' dbWriteTableEM(get("connEM"), Df = mtcars2lSv)
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom caroline dbWriteTable2
#' @importFrom DBI dbExistsTable dbWriteTable
#' @importFrom DBI dbListFields dbGetQuery dbSendQuery dbColumnInfo dbClearResult
#' @importFrom rpostgis dbColumn
#' @export
dbWriteTableEM <- function(conn, DfName = substitute(Df), Df, FillNull = TRUE,
                           AddID = FALSE, RowNames = FALSE, PgUpdateSeq = FALSE,
                           lowerDfName = TRUE, lowerColNames = TRUE, replaceDotUsingUnderscore = TRUE,
                           ...) {
tryCatchLog::tryCatchLog({

  # R CRAN package caroline function dbWriteTable2
  # can not see DBI/RPostgreSQL S4 methods, so I am importing
  # the package "DBI" methods that the package "caroline" uses.
  # #' @importFrom DBI db* ETC

  # Influenced by R CRAN packages
  # "RPostgreSQL",
  # "caroline",
  # and (especially) "rpostgis".
  #   (excellent: but pg* functions require the "PostGIS extension")

  # Influenced by the github/gitlab R packages
  # https://github.com/jangorecki/pg (https://gitlab.com/jangorecki/pg)
  # https://github.com/jangorecki/logR (https://gitlab.com/jangorecki/logR)
  #

  # Note: Please also read
  #
  # dbWriteTable assumes creation (but caroline:dbWriteTable2 assumes "append").
  # Behavior may have changed over time from (original default) "append" to (now) "create"
  #
  # overwrite = TRUE # destroy and re-create
  # append = TRUE (do not "destroy and re-create") # append data
  #
  # DEC 2020
  # RPostgreSQL/html/dbReadTable-methods.html
  # DBI/html/dbWriteTable.html

  Dots <- list(...)

  if(lowerDfName) {
    DfName       <- tolower(DfName)
  }
  if(lowerColNames) {
    colnames(Df) <- tolower(colnames(Df))
  }
  if(replaceDotUsingUnderscore) {
    colnames(Df) <- gsub("[.]", "_", colnames(Df))
  }

  NewDots <- c(list(),Dots, append = TRUE)

  # so the function call can see it them
  # NOTE: this is "per session"
  # dbListFields <<- DBI::dbListFields

  # Because caroline dbWriteTable2 requires the table to pre-exist.
  if(!DBI::dbExistsTable(conn, name = DfName)) {
    # just need the structure (not the data)
    DBI::dbWriteTable(conn, name = DfName, value = Df[FALSE, , drop = F], row.names = RowNames)
  }

  # # https://github.com/tomoakin/RPostgreSQL/blob/master/RPostgreSQL/R/PostgreSQLSupport.R
  # postgresqlTableRef <- function(identifiers){
  #   ret <- paste('"', gsub('"','""',identifiers), '"', sep="", collapse=".")
  #   ret
  # }
  # # dbExecuteEM(conn, paste0("ALTER TABLE ", postgresqlTableRef(DfName), " ADD COLUMN id INTEGER;"))

  # if new columns exist in Df but do not exist at con(remote database)
  # then I MUST add them here to the con
  # *** TO BE IMPLEMENTED

  # STOP BECAUSE THIS IS CRITICAL
  ### ANDRE
  message("Need to Implement case: if the Df has more/different col than the remoted database, \nthen FIRST, add those columns to the database.")
  ### stop("Need to Implement case: if the Df has more/different col than the remoted database, \nthen FIRST, add those columns to the database.")

  # Because caroline dbWriteTable2 requires it.
  # Just (badly) needed to (indirectly) get the column data types.
  # Could have better used: SELECT * FROM name where 1 = 0;
  createdFakeId <- FALSE
  if(!"id" %in% dbListFields(conn, name = DfName)) {
    rpostgis::dbColumn(conn, name = DfName, colname = "id")
    createdFakeId <- TRUE
  }





  # expect the table to already be there
  DescTools::DoCall(
    "caroline::dbWriteTable2", c(list(),
                                 list(conn), list(table.name = DfName), list(df = Df), list(fill.null = FillNull),
                                 list(add.id = AddID), list(row.names = RowNames), list(pg.update.seq = PgUpdateSeq),
                                 NewDots
    )
  )

  if(createdFakeId) {
    DBI::dbExecute(conn, paste0("ALTER TABLE ", postgresqlTableRef(DfName), " DROP COLUMN id;"))
  }
  invisible()

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}




#' PostgreSQL Performance
#'
#' Set PostgreSQL memory parameters.
#'
#' @param conn PostgreSQL DBI connection.
#' @returns PostgreSQL parameters are set
#' @examples
#' \dontrun{
#' dbSetPerformance(get("connEM"))
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbSetPerformance <- function(conn) {
tryCatchLog::tryCatchLog({

  dbGetQueryEM(conn, "SET EFFECTIVE_CACHE_SIZE TO '6144MB';")
  dbGetQueryEM(conn, "SET WORK_MEM TO '2047MB';")
  dbGetQueryEM(conn, "SET MAINTENANCE_WORK_MEM TO '2047MB';")
  dbGetQueryEM(conn, "SET CONSTRAINT_EXCLUSION = ON;")
  dbGetQueryEM(conn, "SET MAX_PARALLEL_WORKERS_PER_GATHER TO 4;")

  return(TRUE)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Test The RPostgreSQL Connection
#'
#' Is the connection not expired or not valid?
#'
#' @param conn PostgreSQL DBI connection.
#' @param ... Dots passed.
#' @returns TRUE(connected) or FALSE(otherwise)
#' @examples
#' \dontrun{
#' isConnectedEM(conn)
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
isConnectedEM <- function(conn, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  if(missing(conn)) {
    stop(paste0("Paramter \"conn\" is required."))
  }

  Results <- list()

  if(inherits(conn, "PostgreSQLConnection")) {

    One <-try({dbGetQueryEM(conn, "SELECT 1;")}, silent = TRUE)
    if(inherits(One, "try-error")) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else {
    return(FALSE)
  }

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}




#' Extra Information About The RPostgreSQL Connection
#'
#' Adds extra information: current_schema, search_path, temp_dbname, econmodel_db_dbname, and timeZone.
#' It will not report the working database (econmodel_db_dbname)  (if it does not yet exist).
#' It will not report the user temporary database (temp_dbname) (if it does not yet exist).
#'
#' @param conn PostgreSQL DBI connection
#' @param ... Dots passed.
#' @returns R list of Strings of properties.
#' @examples
#' \dontrun{
#' dbGetInfoExtraEM(conn)
#' dbGetInfoExtraEM(get("connEM"))
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbGetInfoExtraEM <- function(conn, ...) {
tryCatchLog::tryCatchLog({

  if(missing(conn)) {
    stop(paste0("Paramter \"conn\" is required."))
  }

  Results <- list()

  if(inherits(conn, "PostgreSQLConnection")) {


    One <-try({dbGetQueryEM(conn, "SELECT 1;")}, silent = TRUE)
    if(inherits(One, "try-error")) {
      stop(paste0("Parameter \"conn\" is not a [working] DBI connection."))
    }

    Results["current_schema"] <- unlist(tolower(dbGetQueryEM(conn, "SELECT current_schema();")))

    Results["search_path"]    <- unlist(tolower(dbGetQueryEM(conn, "SHOW SEARCH_PATH;")))
    InterimResult               <- unlist(tolower(dbGetQueryEM(conn, "SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema();")))
    if(length(InterimResult)) {
      Results["temp_dbname"]  <- InterimResult
    }
    if(length(getOption("econmodel_db_dbname"))) {
       Results[["econmodel_db_dbname"]] <- getOption("econmodel_db_dbname")
    }

    Results["timeZone"]       <- unlist(tolower(dbGetQueryEM(conn, "SHOW TIMEZONE;")))
  }

  return(Results)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Connect to the econModel database.
#'
#' Currently is only implemented to work on PostgreSQL or PostgreSQL-like databases.
#'
#' This function tries to return a new "DBI" connection object to the environment "env".
#'
#' First, this function will try to connect to the database using the user name "user" specified from getOption("econmodel_db_*") parameters listed in the "parameters" section.
#'
#' If the connection can be made, then next the connection is tried to be made using the user/password/dbname of "r_user_econmodel".
#' If the connection is made, then next the schema "r_user_econmodel" is attempted to be made..
#'
#' If the connection can not be made, then next the connection is tried to be made using the user/password/dbname of "postgres".
#' If the connection can be made, the the user/password/dbname/schema of "r_user_econmodel" will be made.
#' Next, disconnect. Connect as "r_user_econmodel".
#' Create the user/password/dbname of "user"
#' Next, disconnect. Connect as "user".
#' Create the schema "user".
#' Store the new connection object connName in "env".
#'
#' @param driver String. Defaults to getOption("econmodel_db_driver"). String.  Default is "PostgreSQL".  Currently only an implementation exists using PostgreSQL and PostgreSQL-like databases.
#' @param user String. Defaults to getOption("econmodel_db_user").
#' @param password String. Defaults to "user". If missing, then defaults to getOption("econmodel_db_password") .
#' @param host String. Defaults to getOption("econmodel_db_host").
#' @param dbname String. Defaults to "user". If missing, then defaults to getOption("econmodel_db_dbname").
#' @param port Integer. Defaults to getOption("econmodel_db_port").
#' @param tty Default to getOption("econmodel_db_tty").
#' @param options Defaults to getOption("econmodel_db_dboptions").
#' @param forceISOdate Logical. Default is getOption("econmodel_db_forceISOdate").
#' @param connName String.  Name of the database connection object.  The default is "connEM".
#' @param env Environment.  Default is the .Global environment.  This is the environment to return the connection object "connEM".
#' @param ... Dots passed.
#' @returns DBI connection object named "connEM" is created, connected and assigned to the environment "env".
#' @examples
#' \dontrun{
#' dbConnectEM()
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbConnect
#' @export
dbConnectEM <- function(driver, user, password = user, host, dbname = user, port,
                        tty, options, forceISOdate, connName, env, ...) {
tryCatchLog::tryCatchLog({

  #correct for TZ
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  on.exit({Sys.setenv(TZ=oldtz)})

  if(missing(driver)) {
    driver <- getOption("econmodel_db_driver")
  }
  if(!driver %in% "PostgreSQL") {
    stop("Parameter \"driver\" must be \"PostreSQL\".  No other driver is implemented at this time.")
  }

  if(missing(user)) {
    user <- getOption("econmodel_db_user")
  }
  # note: "password" is never missing
  if(missing(password)) {
    password <- getOption("econmodel_db_password")
  }
  if(missing(host)) {
    host <- getOption("econmodel_db_host")
  }
  # note: "dbname" is never missing
  if(missing(dbname)) {
    dbname <- getOption("econmodel_db_dbname")
  }
  if(missing(port)) {
    port <- getOption("econmodel_db_port")
  }
  if(missing(tty)) {
    tty <- getOption("econmodel_db_tty")
  }
  if(missing(options)) {
    dboptions <- getOption("econmodel_db_dboptions")
  }
  if(missing(forceISOdate)) {
    forceISOdate <- getOption("econmodel_db_forceISOdate")
  }

  if(missing(env)) {
    env <- .GlobalEnv
    ReturnTo <- "the Global environment"
  }
  if(identical(env, .GlobalEnv)) {
    ReturnTo <- "the Global environment"
  } else {
    ReturnTo <- paste0("environment ", capture.output(env))
  }

  if(missing(connName)) {
    connName <- "connEM"
  }

  drv <- try({drv <- DBI::dbDriver(getOption("econmodel_db_driver"))}, silent = TRUE)
  if(!inherits(drv, "try-error")) {

    connNameConnected <- FALSE
    connNameExists      <- exists(connName, envir = env, inherits = FALSE)
    if(connNameExists) {
      connNameConnected <- isConnectedEM(get(connName, envir = env, inherits = FALSE))
    }

    haveConnLocal <- FALSE
    if(connNameExists && connNameConnected) {
      # creates a reference
      conn <- try({get(connName, envir = env, inherits = FALSE)}, silent = TRUE)
    }
    if(!inherits(conn, "try-error")) {
      haveConnLocal <- TRUE
    }

    if( !haveConnLocal
        ||
        (
        !connNameExists
         ||
        (connNameExists && !connNameConnected)
        )
    ) {
      conn <- try({conn <- DBI::dbConnect(drv,
                                         user = user, password = password,
                                         host = host, dbname = dbname, port = port,
                                         tty = tty, options = dboptions, forceISOdate = forceISOdate)},
                  silent = TRUE)
    }
    if(!inherits(conn, "try-error") && dbGetInfo(conn)$user == user) {
      cat(paste0("Successfully connected to user \"", user, "\"\n"))
      if(!dbExistsSchemaEM(conn, schema = user)) dbCreateSchemaEM(conn, schema = user)
      # creates a reference
      assign(connName, conn, envir = env)
      cat(paste0("Connection R object \"connEM\" has been returned to ", ReturnTo, "."))
      # "r_user_econmodel" in the "r_user_econmodel" database
      # tries to create the schema "r_user_econmodel"
      # do not Disconnect
    }
    else {
      # try another connection
      if(haveConnLocal) {
        dbDisconnectEM(conn)
      }
      conn <- try({conn <- DBI::dbConnect(drv,
                                         user = "r_user_econmodel", password = "r_user_econmodel",
                                         host = host, dbname = "r_user_econmodel", port = port,
                                         tty = tty, options = dboptions, forceISOdate = forceISOdate)},
                  silent = TRUE)

      if(!inherits(conn, "try-error")) {

        cat(paste0("Successfully connected to user \"", "r_user_econmodel", "\"\n"))
        if(!dbExistsSchemaEM(conn, schema = "r_user_econmodel")) dbCreateSchemaEM(conn, schema = "r_user_econmodel")
        # user = user

        if(!dbExistsUserEM( conn, user   = user))    dbCreateUserEM(conn,   user   = user)
        if(!dbExistsDbaseEM(conn, dbname = dbname))  dbCreateDbaseEM(conn,  dbname = user)
        dbDisconnectEM(conn)
        #
        # as "ruser"
        # login to the database "user" and and create the "user" schema
        #
        conn <- try({conn <- DBI::dbConnect(drv,
                                            user = user, password = password,
                                            host = host, dbname = user, port = port,
                                            tty = tty, options = dboptions, forceISOdate = forceISOdate)},
                    silent = TRUE)
        if(!inherits(conn, "try-error")) {
          # "user" in the "user" database
          # tries to create the schema "user"
          if(!dbExistsSchemaEM(conn, schema = user)) dbCreateSchemaEM(conn, schema = user)
          dbDisconnectEM(conn)
          # login as "user" to its own database "user"
          dbConnectEM(connName = connName, env = env)
        } else {
          stop(paste0("\"r_user_econmodel\" could not connect to the user database ", user))
        }

      } else {
        # try another connection
        conn <- try({conn <- DBI::dbConnect(drv,
                                           user = "postgres", password = "postgres",
                                           host = host, dbname = "postgres", port = port,
                                           tty = tty, options = dboptions, forceISOdate = forceISOdate)},
                    silent = TRUE)
        if(!inherits(conn, "try-error")) {

          cat(paste0("Successfully connected to user \"", "postgres", "\"\n"))
          if(!dbExistsUserEM( conn, user   = "r_user_econmodel")) dbCreateUserEM(conn, user = "r_user_econmodel", attributes = c("LOGIN", "CREATEDB", "CREATEROLE"))
          if(!dbExistsDbaseEM(conn, dbname = "r_user_econmodel")) dbCreateDbaseEM(conn, dbname = "r_user_econmodel")
          dbDisconnectEM(conn)
          dbConnectEM(driver, user = "r_user_econmodel", password = "r_user_econmodel", host = host, dbname = "r_user_econmodel", port = port,
                      tty = tty, options = dboptions, forceISOdate = forceISOdate, connName = connName, env = env)
        } else {
          stop("Failed to login as getOption(\"econmodel_db_user\"), \"r_user_econmodel\", and \"postgres.\"  Please set options()")
        }
      }
    }
  } else {
    stop(paste0("Parameter \"driver\" is specified as \"", driver, "\".  But the driver failed to load."))
  }

  invisible()

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Executes in the  database
#'
#' Disconnects the R object passed "conn".
#' Alternately, can disconnect and remove the DBI connection object connName (default is "connEM") from the environment "env".
#' The function will just do one or the other, but not both.  Does NOT remove the passed R object "conn".
#'
#' @param conn PostgreSQL DBI connection. Optional. This may be "passed" instead of  "connName".
#' @param Statement String. Required.  DML/DDL/DCL to execute
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @param ... Dots passed.
#' @returns Execution of "conn" from the database or disconnects "connName" from the database and removes it from the environment "env".
#' @examples
#' \dontrun{
#' dbExecuteEM(get("connEM"), "CREATE TABLE xyz();")
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbExecuteEM <- function(conn, Statement, display = TRUE, exec = TRUE, ...) {
tryCatchLog::tryCatchLog({

  #correct for TZ
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  on.exit({Sys.setenv(TZ=oldtz)})

  Dots <- list(...)

  if(missing(conn)) {
    stop("Parameter \"conn\" must be provided.")
  }

  if(missing(Statement)) {
    stop("Parameter \"Statement\" must be provided.")
  }

  ## Build the query
  tmp.query <- Statement

  ## Display the query
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }

  # Execute the query
  if(exec) {
    Results <- try({DBI::dbExecute(conn, tmp.query)}, silent = T)
    if(inherits(Result, "try-error")) {
      stop("Execution of statement \"", tmp.query, "\" failed.")
    } else {
      return(Results)
    }
  }

  invisible()

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}




#' Disconnect from and econModel database
#'
#' Disconnects the R object passed "conn".
#' Alternately, can disconnect and remove the DBI connection object connName (default is "connEM") from the environment "env".
#' The function will just do one or the other, but not both.  Does NOT remove the passed R object "conn".
#'
#' @param conn PostgreSQL DBI connection. Optional. This may be "passed" instead of  "connName".
#' @param connName String.  Name of the database connection object. Optional. This may be "passed" instead of  "conn".
#' @param env Environment. Default is the global environment .GlobalEnv.  Location of the connection object "connName".
#' @param ... Dots passed.
#' @returns Disconnects "conn" from the database or disconnects "connName" from the database and removes it from the environment "env".
#' @examples
#' \dontrun{
#' dbDisconnectEM() # default is connection variable "connEM" in the .GlobalEnv
#' dbDisconnectEM(conn)
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbDisconnectEM <- function(conn, connName, env, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  if(missing(connName)) {
    connName <- "connEM"
  }

  if(missing(env)) {
    env <- .GlobalEnv
  }

  TriedDisconnectedOne <- FALSE
  if(!missing(conn)) {
    try(DBI::dbDisconnect(conn), silent = T)
    cat(paste0("Tried to disconnect the passed R object \"conn\".\n"))
    TriedDisconnectedOne <- TRUE
  }

  assign("connNameName", connName, envir = env)
  if(!TriedDisconnectedOne && exists(connName, envir = env, inherits = FALSE)) {
    with(env, {
      try( DBI::dbDisconnect(get(connNameName)), silent = TRUE)
      # always return TRUE
      #
      rm(list = c(connNameName, "connNameName"))
    })
    cat(paste0("Tried to disconnect the R object \"", connName, "\" and remove it\nfrom the environment ", capture.output(env), ".\n"))
  }

  # DBI::dbDisconnect
  # always returns TRUE
  TRUE
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' User Existence?
#'
#' Determine if a user exists in the database.
#'
#' @param conn PostgreSQL DBI connection. Required.
#' @param user String. Required.  Potential user in the database.
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @param ... Dots passed.
#' @returns TRUE(exists) or FALSE(not exists)
#' @examples
#' \dontrun{
#'  dbExistsUserEM(conn, user = "r_user_econmodel")
#'  dbExistsUserEM(conn, user = "rtmp")
#' }
#' @importFrom tryCatchLog tryCatchLog
dbExistsUserEM <- function(conn, user, display = TRUE, exec = TRUE, ...) {
tryCatchLog::tryCatchLog({

  ## Build the query
  tmp.query <- paste0("SELECT EXISTS(SELECT usename FROM pg_catalog.pg_user WHERE usename = ", DBI::dbQuoteLiteral(conn, user), ");")
  ## Display the query
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }

  ## Execute the query and return TRUE
  if (exec) {
    Result <- dbGetQueryEM(conn, tmp.query)
    if(unlist(Result)) {
     return(TRUE)
    } else {
     return(FALSE)
    }
  }

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' User Creation
#'
#' Create a user exists in the database.
#'
#' @param conn PostgreSQL DBI connection. Required.
#' @param user String. Required.  Potential user in the database.
#' @param attributes  vector of Strings. User attributes.
#' @param password String. Defaults to "user".
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @param ... Dots Passed.
#' @returns TRUE(success) or Error(failure)
#' @examples
#' \dontrun{
#' dbCreateUserEM(conn, user = "r_user_econmodel", attributes = c("LOGIN", "CREATEDB", "CREATEROLE"))
#' dbCreateUserEM(conn, user = "rtmp")
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbExecute dbQuoteLiteral
dbCreateUserEM <- function(conn, user, attributes = c("LOGIN"), password = user, display = TRUE, exec = TRUE, ...) {
tryCatchLog::tryCatchLog({

  if(missing(conn)) {
    stop("Parameter \"conn\" is required.")
  }
  if(missing(user)) {
    stop("Parameter \"user\" is required.")
  }
  if(length(attributes)) {
    attributes <- paste0(attributes , collapse = " ")
  }
  if(missing(password)) {
    password <- character()
  } else {
    password <- paste0("password ", DBI::dbQuoteLiteral(conn, password))
  }

  ## Build the query
  tmp.query <- paste0("CREATE ROLE ", user, " ", attributes, " NOINHERIT ", password, ";")
  ## Display the query
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }

  ## Execute the query and return TRUE
  if (exec) {
    if(!dbExistsUserEM(conn, user)) {
      Result <- try({Result <- dbExecuteEM(conn, tmp.query)})
      if(!inherits(Result, "try-error")) {
        return(TRUE)
      } else {
        stop("Failed to create the user.")
      }
    } else {
      stop(paste0("User ", DBI::dbQuoteLiteral(conn, user), " is already in the database."))
    }
  }

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Schema Existence?
#'
#' Determine if a schema exists in the database.
#'
#' @param conn PostgreSQL DBI connection. Required.
#' @param schema String. Required.  Potential schema in the database.
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @param ... Dots passed.
#' @returns TRUE(exists) or FALSE(not exists)
#' @examples
#' \dontrun{
#'  dbExistsSchemaEM(conn, schema = "r_user_econmodel")
#'  dbExistsSchemaEM(conn, schema = "rtmp")
#' }
#' @importFrom tryCatchLog tryCatchLog
dbExistsSchemaEM <- function(conn, schema, display = TRUE, exec = TRUE, ...) {
tryCatchLog::tryCatchLog({

  ## Build the query
  tmp.query <-  paste0("SELECT EXISTS(SELECT 1 FROM pg_catalog.pg_namespace WHERE nspname = ", DBI::dbQuoteLiteral(conn, schema), ");")
  ## Display the query
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }

  ## Execute the query and return TRUE
  if (exec) {
    Result <- dbGetQueryEM(conn, tmp.query)
    if(unlist(Result)) {
     return(TRUE)
    } else {
     return(FALSE)
    }
  }

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Schema Creation
#'
#' Create a schema in the database.
#'
#' @param conn PostgreSQL DBI connection. Required.
#' @param schema String. Required.  Potential schema in the database.
#' @param role_specification.  String. The schema role specification.  Defaults to "schema".
#' @param grant_all vector of Strings. Roles to be GRANT ALLed to this schema.  Defaults to "schema".
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @param ... Dots passed.
#' @returns TRUE(success) or Error(failure)
#' @examples
#' \dontrun{
#'  dbCreateSchemaEM(conn, schema = "r_user_econmodel")
#'  dbCreateSchemaEM(conn, schema = "rtmp")
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbExecute dbQuoteLiteral
dbCreateSchemaEM <- function(conn, schema, role_specification = schema, grant_all_roles = schema, display = TRUE, exec = TRUE, ...) {
tryCatchLog::tryCatchLog({

  if(missing(conn)) {
    stop("Parameter \"conn\" is required.")
  }

  if(1L < NROW(role_specification)) {
    stop("Parameter \"role_specification\" can only have one role.")
  }

  ## Build the query
  tmp.query <- paste0("CREATE SCHEMA ", schema, " AUTHORIZATION ", role_specification, ";")
  ## Display the query
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }

  ## Execute the query and return TRUE
  if (exec) {
    if(!dbExistsSchemaEM(conn, schema)) {
      Result <- try({Result <- dbExecuteEM(conn, tmp.query)})
      if(inherits(Result, "try-error")) {
        stop("Failed to create the schema.")
      }
    } else {
      stop(paste0("Schema ", DBI::dbQuoteLiteral(conn, user), " is already in the database."))
    }
  }

  lapply(grant_all_roles, function(grant_all_role) {

    ## Build the query
    tmp.query <- paste0("GRANT ALL ON SCHEMA ", schema, " TO ", grant_all_role, ";")
    ## Display the query
    if (display) {
      message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
      message(tmp.query)
    }
    ## Execute the query and return TRUE
    if (exec) {
      Result <- try({Result <- dbExecuteEM(conn, tmp.query)})
      if(inherits(Result, "try-error")) {
        stop(paste0("Failed to grant all on the schema to ", grant_all_role))
      }
    }

  })

  TRUE

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Database Existence?
#'
#' Determine if a database exists in the cluster.
#'
#' @param conn PostgreSQL DBI connection. Required.
#' @param dbname String. Required.  Potential database in the cluster.
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @param ... Dots passed.
#' @returns TRUE(exists) or FALSE(not exists)
#' @examples
#' \dontrun{
#'  dbExistsDbaseEM(conn, dbname = "r_user_econmodel")
#'  dbExistsDbaseEM(conn, dbname = "rtmp")
#' }
#' @importFrom tryCatchLog tryCatchLog
dbExistsDbaseEM <- function(conn, dbname, display = TRUE, exec = TRUE, ...) {
tryCatchLog::tryCatchLog({

  ## Build the query
  tmp.query <- paste0("SELECT EXISTS(SELECT 1 FROM pg_catalog.pg_database WHERE datname = ", DBI::dbQuoteLiteral(conn, dbname), ");")
  ## Display the query
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }

  ## Execute the query and return TRUE
  if (exec) {
    Result <- dbGetQueryEM(conn, tmp.query)
    if(unlist(Result)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}




#' Database Creation
#'
#' Create a a database in the cluster.
#'
#' @param conn PostgreSQL DBI connection. Required.
#' @param dbname String. Required.  Potential database in the cluster.
#' @param owner String. Database owner. Defaults to parameter "dbname".
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @param ... Dots Passed.
#' @returns TRUE(success) or Error(failure)
#' @examples
#' \dontrun{
#' dbCreateDbaseEM(conn, dbname = "r_user_econmodel")
#' dbCreateDbaseEM(conn, dbname = "rtmp")
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbExecute dbQuoteLiteral
dbCreateDbaseEM <- function(conn, dbname, owner = dbname, display = TRUE, exec = TRUE, ...) {
tryCatchLog::tryCatchLog({

  if(missing(conn)) {
    stop("Parameter \"conn\" is required.")
  }
  if(missing(dbname)) {
    stop("Parameter \"dbname\" is required.")
  }

  # Note: SUPERUSER postgres - does not require THIS
  # (but I am not "filtering out" SUPERUSERs)
  #
  # “ERROR: must be member of role” When creating schema in PostgreSQL
  # 2014
  # https://stackoverflow.com/questions/26684643/error-must-be-member-of-role-when-creating-schema-in-postgresql
  CurrentUser <- unlist(dbGetQueryEM(conn, "SELECT CURRENT_USER;"))
  tmp.query <- paste0("GRANT ", owner, " TO ", CurrentUser, ";")
  ## Display the query
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }
  ## Execute the query and return TRUE
  if (exec) {
    Result <- try({Result <- dbExecuteEM(conn, tmp.query) })
    if (inherits(Result, "try-error")) {
      stop(paste0("Failed to grant: ", tmp.query))
    }
  }

  # Note: SUPERUSER postgres - does not require THIS
  # (but I am not "filtering out" SUPERUSERs)
  #
  # permission denied for tablespace pg_default
  #
  # Why do I get a PostgreSQL permission error when specifying a tablespace in the “create database” command?
  #
  # . . .
  # Here is(sp) the trick . . .  Leave the tablespace blank.
  # It will default to "pg_default" when creating the database.
  # 2017
  # https://dba.stackexchange.com/questions/204807/why-do-i-get-a-postgresql-permission-error-when-specifying-a-tablespace-in-the
  #
  # SO omitting
  # TABLESPACE = pg_default

  # Note to "create the database AND with assigned owner, the owner (obviously) must pre-exist"
  # I "could test" to determine the needed pre-existing owner (but I am not testing)
  #
  ## Build the query
  RestOfDbCreate <- "ENCODING  = 'UTF-8' LC_COLLATE = 'C' LC_CTYPE  = 'C' CONNECTION_LIMIT = -1"
  tmp.query <- paste0("CREATE DATABASE ", dbname, " ", " WITH OWNER ", owner, " ",  RestOfDbCreate, ";")
  ## Display the query
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }
  ## Execute the query and return TRUE
  if (exec) {
    if(!dbExistsDbaseEM(conn, dbname)) {
      Result <- try({Result <- dbExecuteEM(conn, tmp.query)})
      if(!inherits(Result, "try-error")) {
        return(TRUE)
      } else {
        stop("Failed to create the database.")
      }
    } else {
      stop(paste0("Database ", DBI::dbQuoteLiteral(conn, dbname), " is already in the cluster."))
    }
  }

  tmp.query <- paste0("ALTER DATABASE ", dbname, " SET TIME ZONE 'UTC';")
  ## Display the query
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }
  ## Execute the query and return TRUE
  if (exec) {
    if(!dbExistsDbaseEM(conn, dbname)) {
      Result <- try({Result <- dbExecuteEM(conn, tmp.query)})
      if(!inherits(Result, "try-error")) {
        return(TRUE)
      } else {
        stop("Failed to alter the database.")
      }
    } else {
      stop(paste0("Database ", DBI::dbQuoteLiteral(conn, dbname), " can not be alterned from here."))
    }
  }

  invisible()

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' From the Cluster, Remove Old Work Databases
#'
#' In the cluster, logs on to the database "r_user_econmodel" as user "rtmp%" and (tries to) drop the "rtmp%" database.
#' Does not drop the "current work database": "getOption("econmodel_db_dbname")".
#'
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @param Dots passed.
#' @returns TRUE(always).  Will attempt to drop each 'rtmp%' database (but not the current work database).
#' @examples
#' \dontrun{
#' clRemoveOldWorkDbasesEM()
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbDriver dbConnect dbGetQuery, dbQuoteLiteral dbDisconnect
#' @export
clRemoveOldWorkDbasesEM <- function(display = TRUE, exec = TRUE, ...) {
tryCatchLog::tryCatchLog({

  ops <- options()
  options(warn = 1L)

  Dots <- list(...)

  drv  <- DBI::dbDriver("PostgreSQL")
  conn <- DBI::dbConnect(drv, user = "r_user_econmodel", password = "r_user_econmodel", dbname = "r_user_econmodel")
  NotDatabase <- DBI::dbQuoteLiteral(conn, getOption("econmodel_db_dbname"))
  tmp.query <-  paste0("SELECT datname FROM pg_catalog.pg_database WHERE datname LIKE 'rtmp%' AND datname != ", NotDatabase, ";")
  ## Display the query
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }
  ## Execute the query
  if(exec) {
    Databases   <- dbGetQueryEM(conn, tmp.query)
  }
  Databases   <- unlist(Databases)

  lapply(Databases, function(dbname) {
    try({
      # I can not be "in" the database I am trying to drop
      # Avoid the error: ERROR:  cannot drop the currently open database
      conn <- DBI::dbConnect(drv, user = dbname, password = dbname, dbname = "r_user_econmodel")
      # trying to drop my own database
      tmp.query <- paste0("DROP DATABASE ", dbname, ";")
      ## Display the query
      if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(tmp.query)
      }
      ## Execute the query
      if(exec) {
        Results <- dbExecuteEM(conn, tmp.query)
        if(inherits(Results, "try-error")) {
          message(paste0("Failed to drop database ", dbname, "."))
        }
      }
      dbDisconnectEM(conn)

    }, silent = TRUE)
    invisible()
  })

  options(ops)
  return(TRUE)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}





