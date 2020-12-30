
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

  Sys.setenv(TZ=oldtz)

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
#' @param ...	Other parameters passed on to methods.
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbGetQuery
#' @export
dbGetQueryEm <- function(conn, Statement, ...) {
tryCatchLog::tryCatchLog({

  Results <- DBI::dbGetQuery(conn, statement = Statement, ...)
  colnames(Results) <- toupper(colnames(Results))
  return(Results)

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
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom caroline dbWriteTable2
#' @importFrom DBI dbExistsTable dbWriteTable
#' @importFrom DBI dbListFields dbGetQuery dbSendQuery dbColumnInfo dbClearResult
#' @importFrom rpostgis dbColumn
#' @export
dbWriteTableEm <- function(conn, DfName = substitute(Df), Df, FillNull = TRUE,
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
  # # DBI::dbExecute(conn, paste0("ALTER TABLE ", postgresqlTableRef(DfName), " ADD COLUMN id INTEGER;"))

  # if new columns exist in Df but do not exist at con(remote database)
  # then I MUST add them here to the con
  # *** TO BE IMPLEMENTED

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




