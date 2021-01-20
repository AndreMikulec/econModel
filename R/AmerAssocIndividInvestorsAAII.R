
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
#' # E.g. Installers location
#' # C:\DATA\AAIIStockInvestorProInstallers
#'
#' # From installing one version to the next, older folders/files will remain
#' # NOTE, Recommended Best Practice
#' # Before each install, delete all of the folders and files in
#' # C:\Program Files (x86)\Stock Investor\Professional
#'
#' # Note, the SETUP.DBF file date is always the "as of software distribution".
#'
#' #
#' # First, the user must manually create the folder C:\DATA\AAIISIPRO\AUXILIARY
#' #
#'
#' # # #
#' # - Change to internal company identifier and new monthly Stock
#' #   Investor News email (7/29/2011 Release)
#' # ReadMe.txt
#' # # #
#'
#' # Last good before the change of COMPANY_ID
#' # proinstall_20110722.exe (15177)
#'
#' copyAAIISIProDBFs(
#'   From = "C:\\Program Files (x86)\\Stock Investor\\Professional",
#'   To   = paste0("C:\\DATA\\AAIISIPRO\\AUXILIARY","\\", dateAAIISIPro()),
#'   CaseChange = "UpperCase"
#' )
#'
#' # The change of of COMPANY_ID
#' # proinstall_20110729.exe (15184)
#'
#' copyAAIISIProDBFs(
#'   From = "C:\\Program Files (x86)\\Stock Investor\\Professional",
#'   To   = paste0("C:\\DATA\\AAIISIPRO\\AUXILIARY","\\", dateAAIISIPro()),
#'   CaseChange = "UpperCase"
#' )
#'
#' # # #
#' # - New sector and industry classifications (10/22/2018 release)
#' # ReadMe.txt
#' # # #
#'
#' # Last good before the change of Sectors and Industries
#' # stockinvestorinstall_20181019.exe (17823)
#'
#' copyAAIISIProDBFs(
#'   From = "C:\\Program Files (x86)\\Stock Investor\\Professional",
#'   To   = paste0("C:\\DATA\\AAIISIPRO\\AUXILIARY","\\", dateAAIISIPro()),
#'   CaseChange = "UpperCase"
#' )
#'
#' # The change of Sectors and Industries
#' # stockinvestorinstall_20181022.exe (17826)
#'
#' copyAAIISIProDBFs(
#'   From = "C:\\Program Files (x86)\\Stock Investor\\Professional",
#'   To   = paste0("C:\\DATA\\AAIISIPRO\\AUXILIARY","\\", dateAAIISIPro()),
#'   CaseChange = "UpperCase"
#' )
#'
#' # # #
#' # Regular 'end of month' installs (from earliest to latest)
#' # # #
#'
#' # e.g.
#' # stockinvestorinstall_20201030.exe (18565)
#' # stockinvestorinstall_20201130.exe (18596)
#' # stockinvestorinstall_20201231.exe (18627)
#'
#' copyAAIISIProDBFs(
#'   From = "C:\\Program Files (x86)\\Stock Investor\\Professional",
#'   To   = paste0("C:\\DATA\\AAIISIPRO\\MONTHDATE","\\", dateAAIISIPro()),
#'   CaseChange = "UpperCase"
#' )
#'
#' # e.g.
#'
#' copyAAIISIProDBFs(
#'   From = "C:\\Program Files (x86)\\Stock Investor\\Professional",
#'   To   = paste0("C:\\DATA\\AAIISIPRO\\MONTHDATE","\\", dateAAIISIPro()),
#'   CaseChange = "UpperCase"
#' )
#'
#' dir(paste0("C:\\DATA\\AAIISIPRO\\MONTHDATE", "\\", dateAAIISIPro()))
#'
#' # # #
#' # create "cleaned up" FST files
#' # # #
#'
#' formatDBFs(paste0("C:/DATA/AAIISIPRO/AUXILIARY","/", 15177))
#'         # Note file SI_TRBCS.DBF does not exist
#' # - Change to internal company identifier and new monthly Stock
#' #   Investor News email (7/29/2011 Release)
#' formatDBFs(paste0("C:/DATA/AAIISIPRO/AUXILIARY","/", 15184))
#'
#' formatDBFs(paste0("C:/DATA/AAIISIPRO/AUXILIARY","/", 17823))
#' # - New sector and industry classifications (10/22/2018 release)
#' formatDBFs(paste0("C:/DATA/AAIISIPRO/AUXILIARY","/", 17826))
#'         # Note file SI_TRBCS.DBF appears
#'
#' formatDBFs(paste0("C:/DATA/AAIISIPRO/MONTHDATE","/", 18565))
#' formatDBFs(paste0("C:/DATA/AAIISIPRO/MONTHDATE","/", 18596))
#' formatDBFs(paste0("C:/DATA/AAIISIPRO/MONTHDATE","/", 18627))
#'
#' # View some
#' # e.g.
#' # viewSIPRO("Base", Source = "Repository", SubDir = "C:\\DATA\\AAIISIPRO\\MONTHDATE\\18565", Ext = "FST")
#'
#' # TO BE CONTINUED
#' # Use dbWriteTableEM to load the FST files into the PostgreSQL database
#' # # NEED
#' # 0. Partition detection\creation db*x* functions.
#' # 1. NEED dbWriteTableEM to read FST files into local "Df"s
#' #         NEED "inital empty table" creation to be a paritioned table
#' #         NEED ListPartitionCols = c()
#' # 2. dbWriteTableEnMassEM
#' }
#' @importFrom tryCatchLog tryCatchLog
copyAAIISIProDBFs <- function(From = "C:\\Program Files (x86)\\Stock Investor\\Professional",
                              To = tempdir(),
                              CaseChange = "UpperCase") {
tryCatchLog::tryCatchLog({

  From <- normalizePath(From, winslash = "/")

  if(!dir.exists(To)) {
    dir.create(To, showWarnings = FALSE)
  }
  # normalizePath, first "Checks" to make sure that the directory exists
  # If the directory "To" does not already exist
  # then [later] R.utils__copyDirectoryByPattern will create it.
  To   <- normalizePath(To, winslash = "/", mustWork = FALSE)

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
#' @param Collection String.  Some of "Base", "Prices", "QuarterlySheets", "AnnualSheets", "DetailedPrices", "Predictive", or "Dictionary". Required.
#' @param Source String.  One of "Install" or "Repository".
#' @param SubDir String.  Single directory containing the "DBF" files. If "Source" = "Repository", then Required. Otherwise, ignored.
#' @param Ext String. Default is "DBF". This is always the case when "Source = "Install": so, this parameters is ignored.  Alternatively, this parameter can be "FST" when "Source = "Repository"".
#' @returns String. Name of the data.frame loaded. data.frames are loaded into the environment.  Attempted to be started is View() upon each data.frame.
#' @examples
#' \dontrun{
#' viewSIPRO("Base")
#' viewSIPRO("Base", Source = "Repository", SubDir = "C:\\DATA\\AAIISIPRO\\MONTHDATE\\18627")
#' viewSIPRO("Base", Source = "Repository", SubDir = "C:\\DATA\\AAIISIPRO\\MONTHDATE\\18627", Ext = "FST")
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

  if( !Collection %in% c("Base", "Prices", "QuarterlySheets", "AnnualSheets", "DetailedPrices", "Predictive", "Dictionary")) {
    stop("Parameter \"Collection\" must be some of \"Base\", \"Prices\", \"QuarterlySheets\", \"AnnualSheets\", \"DetailedPrices\", \"Predictive\", or \"Dictionary\".")
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
  FullCollection[["QuarterlySheets"]] <- c(
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_isq.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_bsq.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_cfq.dbf"
  )

  # Needs "Base"
  FullCollection[["AnnualSheets"]] <- c(
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_isa.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_bsa.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Static\\si_cfa.dbf"
  )

  # Needs "Base"
  FullCollection[["DetailedPrices"]] <- c(
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Dbfs\\si_psda.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Dbfs\\si_psdh.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Dbfs\\si_psdl.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Dbfs\\si_psdv.dbf"
  )

  FullCollection[["Predictive"]] <- c(
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Dbfs\\si_avg.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Dbfs\\si_ee.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Dbfs\\si_gr.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Dbfs\\si_val.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Dbfs\\si_mgavg.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Dbfs\\si_mgav2.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Dbfs\\si_perc.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Dbfs\\si_rat.dbf",
    "C:\\Program Files (x86)\\Stock Investor\\Professional\\Dbfs\\si_mlt.dbf"
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
#' @param PrefixCols Named vector of Strings. Names represent the table names and values represents the column names.  New column names will have a pre-pended values that is the last characters of the table name that follow the table name "_".  Value of "*" means "all columns".
#' @param RemoveCols Vector of regular expressions(PERL = T) of columns to remove.
#' @param RemoveDupsColFileExceptions Files to non-remove duplicate column values.  See the next parameter. RemoveDupsColValues.
#' @param RemoveDupsColValues Column name to have its duplicates (and an corresponding non-duplicates) removed.
#' @param ChangeType list of named vectors, with the name of the vector to be the output datatype, and the values of the vectors to be regular expressions identifying the columns to be converted.  Remaining columns not yet converted are converted to numeric.
#' @return If "From" is a directory, then new files are placed on disk. Alternately, if "From" is an R list, then return a list of modified data.tables.
#' @examples
#' \dontrun{
#' formatDBFs()
#' formatDBFs(paste0("C:\\DATA\\AAIISIPRO\\MONTHDATE","\\", 18627))
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
                                     "SI_DOW.DBF",
                                     # Prices
                                     "SI_PSD.DBF", "SI_PSDC.DBF", "SI_PSDD.DBF",
                                     # Quarterly Sheets
                                     "SI_ISQ.DBF", "SI_BSQ.DBF" , "SI_CFQ.DBF",
                                     # Annual(or Yearly) Sheets
                                     "SI_ISA.DBF", "SI_BSA.DBF" , "SI_CFA.DBF",
                                     # Detailed Prices
                                     "SI_PSDA.DBF", "SI_PSDH.DBF", "SI_PSDL.DBF", "SI_PSDV.DBF",
                                     # Predictive (stats and opinions)
                                     "SI_AVG.DBF", "SI_EE.DBF", "SI_GR.DBF", "SI_VAL.DBF",
                                     # Predictive (sectors and industries and ranks)
                                     "SI_MGAVG.DBF", "SI_MGAV2.DBF", "SI_PERC.DBF",
                                     # Predictive (ratio and multiples)
                                     "SI_RAT.DBF", "SI_MLT.DBF"
                                     ),
                       To = From,
                       PrependColFile = "SETUP.DBF",
                       PrefixCols = c(SI_CI = "LASTMOD",
                                      SI_MGDSC = "MG_CODE", SI_MGDSC = "MG_DESC",
                                      SI_TRBCS = "MG_CODE", SI_TRBCS = "MG_DESC",
                                      SI_PTYP  = "TYPE_CODE", SI_PTYP = "TYPE_DESCR",
                                      SI_UTYP  = "TYPE_CODE", SI_UTYP = "TYPE_DESCR",
                                      SI_MLT   = "PE",
                                      SI_MGAVG = "*", SI_MGAV2 = "*"
                                    ),                                 # only one that has different dates
                       RemoveCols = c("^X.*$", "X_NullFlags", "REPNO", "(?<!CI_)LASTMOD", "UPDATED"),
                       # because the "UTYP" codes (re-statements) are in here
                       RemoveDupsColFileExceptions = c(""), # SI_DATE.DBF only has the most recent 'statement' (so no DUPS)
                       RemoveDupsColValues = c("COMPANY_ID"),
                                                # Dates seem to be already Dates (NOTHING TO DO)
                       ChangeType = list(Date = c("^.*DATE$", "^PRICED.*$", "^.*DT$", "^.*LASTMOD$", "^PEREND_.*$",
                                                  "^DATE_EY0$", "^DATE_EQ0$"
                                                  ),
                                                   # Logicals seem to be already Logicals (NOTHING TO DO)
                                         logical = c("^ADR$", "^OPTIONABLE$", "^DRP_AVAIL$", "^UPDATED$"),
                                         integer = c("^EMPLOYEES$", "^PERLEN_.*$", "^SHRINSTN$"),
                                                     # Characters seem to be already Characters (NOTHING TO DO)
                                         character = c("^COMPANY_ID$", "^COMPANY$", "^TICKER$", "^EXCHANGE$", "^STREET$", "^CITY$", "^STATE$", "^ZIP$",
                                                      "^COUNTRY$", "^PHONE$", "^WEB_ADDR$", "^BUSINESS$", "^ANALYST_FN$", "IND_2_DIG", "^IND_3_DIG$", "^SIC$", "^SP$", "^DOW$",
                                                      "^EXCHG_CODE$", "^EXCHG_DESC$", "^.*MG_CODE$", "^.*MG_DESC$",
                                                      "^PERTYP_.*$", "^UPDTYP_.*$",
                                                      "^SP_CODE$", "^SP_DESC$", "^.*TYPE_CODE$", "^.*TYPE_DESCR$", "^TYPE_SHORT$",
                                                      "^DOW_CODE$", "^DOW_DESC$",
                                                      "^FIELD_NAME")

                                        )
                       ) {
tryCatchLog::tryCatchLog({

  # consistency
  From <- normalizePath(From, winslash = "/")
  To   <- normalizePath(To,   winslash = "/")

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

      if(!file.exists(PathandFile)){
        # temporily a different function call, so I can see the red colors
        cat(paste0("****************************************************************\n"))
        cat(paste0("*** File does not exist: ", PathandFile,", so skipping . . . ***\n"))
        cat(paste0("*** File does not exist: ", PathandFile,", so skipping . . . ***\n"))
        cat(paste0("*** File does not exist: ", PathandFile,", so skipping . . . ***\n"))
        cat(paste0("*** File does not exist: ", PathandFile,", so skipping . . . ***\n"))
        cat(paste0("*** File does not exist: ", PathandFile,", so skipping . . . ***\n"))
        cat(paste0("****************************************************************\n\n"))
        next
      }

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
            if(ColName != "*") {
              cat(paste0("Renaming: ", PathandFile, " column: ", ColName, " name to ", paste0(NewPreFix, "_", colnames(ReadFile)[colnames(ReadFile) == ColName]), ". . . . "))
              colnames(ReadFile)[colnames(ReadFile) == ColName] <<- paste0(NewPreFix, "_", colnames(ReadFile)[colnames(ReadFile) == ColName])
            } else { # rename all columns
              cat(paste0("Renaming: ", PathandFile, " columns: ", paste0(colnames(ReadFile), collapse = ", "), " names to ", paste0(paste0(NewPreFix, "_", colnames(ReadFile)[TRUE             ]), collapse = ", "), ". . . . "))
              colnames(ReadFile)[TRUE                         ] <<- paste0(NewPreFix, "_", colnames(ReadFile)[TRUE                         ])
            }
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
#' @param Statement	a character string containing SQL. The statement "SELECT 1;" will test if a database connection is non-expired or is-valid. Then, the returned value is either data.frame(DBISCONNECTEDEM = TRUE) or data.frame(DBISCONNECTEDEM = TRUE).
#' @param time_zone Execution time zone. Default is "UTC".
#' @param client_encoding Execution encoding. Default is "UTF8".
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbGetQuery
#' @export
dbGetQueryEM <- function(conn, Statement, time_zone = "UTC", client_encoding = "UTF8", display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  if(missing(conn)) {
    stop("Parameter \"conn\" must be provided.")
  }

  if(missing(Statement)) {
    stop("Parameter \"Statement\" must be provided.")
  }

  oldtz <- Sys.getenv("TZ")
  Sys.setenv(TZ=time_zone)
  on.exit({Sys.setenv(TZ=oldtz)})

  DBI::dbExecute(conn, statement =  paste0("SET client_encoding TO '", client_encoding, "';"))

  tmp.query <- Statement
  ## Display the query
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }

  if(exec) {
    Results <- try({DBI::dbGetQuery(conn, statement = tmp.query)}, silent = TRUE)
  }
  if(exec && !inherits(Results, "try-error")) {
    # catch all
    return(data.frame(Results))
  } else if(!exec) {
    return(invisible(data.frame(DBGETQUERYEM = logical())))
  } else if(exec && inherits(Results, "try-error")) {
     # SPECIAL
     stop(paste0("Statement failed: ", tmp.query))
     # return(data.frame(DBGETQUERYEM = logical()))
  } else {}

  return(invisible(data.frame(DBGETQUERYEM = logical())))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}











#' PostgreSQL Performance
#'
#' Set PostgreSQL memory parameters.
#'
#' @param conn PostgreSQL DBI connection.
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @returns PostgreSQL parameters are set
#' @examples
#' \dontrun{
#' dbSetPerformanceEM(get("connEM"))
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbSetPerformanceEM <- function(conn, exec = TRUE, display = TRUE) {
tryCatchLog::tryCatchLog({

  if(missing(conn)) {
    stop("Parameter \"conn\" is required.")
  }

  # DELL HOME "COMPUTER" ( 2017 / WINDOWS 10 PROFESSIONAL )
  # HAS 16GB of RAM AND 4 CORES

  # EXPERIMENT ( memory for disk caching ) -- 2048(GUESSING) + 4096(shared buffers)
  dbExecuteEM(conn, Statement = "SET effective_cache_size  TO '6144MB';", display = display, exec = exec)

  # windows LIMIT 2047
  # A good rule of thumb is to keep: work_mem*max_connections*2 < 1/4 of memory
  # NOTE: WATCH OUT FOR 'R language: parallel. Each process GETS 'work_mem' limit
  dbExecuteEM(conn, Statement = "SET work_mem TO '2047MB';", display = display, exec = exec)

  # maximum amount of memory to be used by maintenance operations, such as VACUUM, CREATE INDEX, and ALTER TABLE ADD FOREIGN KEY
  # https://wiki.postgresql.org/wiki/Tuning_Your_PostgreSQL_Server
  dbExecuteEM(conn, Statement = "SET maintenance_work_mem TO '2047MB';", display = display, exec = exec)

  # Controls the query planner's use of table constraints
  # to optimize queries.
  dbExecuteEM(conn, Statement = "SET constraint_exclusion TO partition;", display = display, exec = exec)

  # excludes (prunes) the partition from the query plan
  # can also be applied [not only to query planning and] during query execution
  dbExecuteEM(conn, Statement = "SET enable_partition_pruning TO on;", display = display, exec = exec)

  # Postgresql 9.6
  dbExecuteEM(conn, Statement = "SET max_parallel_workers_per_gather TO 4;", display = display, exec = exec)

  # always TRUE
  return(invisible(data.frame(DBSETPERFORMANCEEM = TRUE)))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' PostgreSQL Current User
#'
#' Get PostgreSQL CURRENT_USER.
#'
#' @param conn PostgreSQL DBI connection.
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @returns Current user
#' @examples
#' \dontrun{
#' dbGetCurrentuserEM(get("connEM"))
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbGetCurrentuserEM <- function(conn, exec = TRUE, display = TRUE) {
tryCatchLog::tryCatchLog({

  if(missing(conn)) {
    stop("Parameter \"conn\" is required.")
  }

  Results <- dbGetQueryEM(conn, Statement = "SELECT CURRENT_USER;", display = display, exec = exec)
  if(exec) {
    if(NROW(Results)) {
      colnames(Results)[1] <- "DBCURRENTUSEREM"
      return(Results)
    } else {
      stop("The Current user is not being returned fromt the dataase.")
    }
  }
  return(invisible(data.frame(DBCURRENTUSEREM = logical())))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Test The RPostgreSQL Connection
#'
#' Is the connection not expired or not valid?
#'
#' @param conn PostgreSQL DBI connection.
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @returns TRUE(connected) or FALSE(otherwise)
#' @examples
#' \dontrun{
#' isConnectedEM(conn)
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbIsConnectedEM <- function(conn, display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  if(missing(conn)) {
    stop(paste0("Parameter \"conn\" is required."))
  }

  Statement <- "SELECT 1;"

  if(inherits(conn, "PostgreSQLConnection")) {

    tmp.query <- Statement
    ## Display the query
    if (display) {
      message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
      message(tmp.query)
    }

    if(exec) {
      Results <- try({DBI::dbGetQuery(conn, statement = tmp.query)}, silent = TRUE)
    }
    Results <- try({DBI::dbGetQuery(conn, statement = tmp.query)}, silent = TRUE)
    if(exec && !inherits(Results, "try-error")) {
      return(data.frame(DBISCONNECTEDEM = TRUE))
    } else if(!exec) {
      return(invisible(data.frame(DBISCONNECTEDEM = logical())))
    } else if(exec && inherits(Results, "try-error")) {
      # not a connection
      return(data.frame(DBISCONNECTEDEM = FALSE))
    } else {}

  } else {
    stop("Need a \"PostgreSQLConnection\"")
  }

  return(invisible(data.frame(DBISCONNECTEDEM = logical())))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}




#' Extra Information About The RPostgreSQL Connection
#'
#' Adds extra information: current_schema, search_path, temp_dbname, econmodel_db_dbname, client_encoding, and time_zone.
#' It will not report the working database (econmodel_db_dbname)  (if it does not yet exist).
#' It will not report the user temporary database (temp_dbname) (if it does not yet exist).
#'
#' @param conn PostgreSQL DBI connection
#' @param display	Logical. Whether to display the query (defaults to TRUE).
#' @param exec	Logical. Whether to execute the query (defaults to TRUE).
#' @returns R list of Strings of properties.
#' @examples
#' \dontrun{
#' dbGetInfoExtraEM(conn)
#' dbGetInfoExtraEM(get("connEM"))
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbGetInfoExtraEM <- function(conn, display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  if(missing(conn)) {
    stop(paste0("Paramter \"conn\" is required."))
  }

  Results <- list()

  if(inherits(conn, "PostgreSQLConnection")) {

    Results["current_schema"] <- unlist(tolower(dbGetQueryEM(conn, Statement = "SELECT current_schema();", display = display, exec = exec)))
    Results["search_path"]    <- unlist(tolower(dbGetQueryEM(conn, Statement = "SHOW SEARCH_PATH;", display = display, exec = exec)))
    InterimResult             <- unlist(tolower(dbGetQueryEM(conn, Statement = "SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema();", display = display, exec = exec)))
    if(length(InterimResult)) {
      Results["temp_dbname"]  <- InterimResult
    }
    if(length(getOption("econmodel_db_dbname"))) {
       Results[["econmodel_db_dbname"]] <- getOption("econmodel_db_dbname")
    }
    Results["client_encoding"] <- unlist(tolower(dbGetQueryEM(conn, Statement = "SHOW client_encoding;", display = display, exec = exec)))
    Results["time_zone"] <- unlist(tolower(dbGetQueryEM(conn, Statement = "SHOW TIMEZONE;", display = display, exec = exec)))
  } else {
    stop("Need a \"PostgreSQLConnection\"")
  }

  Results <- data.frame(Results)
  colnames(Results) <- toupper(colnames(Results))
  return(Results)

  logical()

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Connect to the econModel database.
#'
#' Currently is only implemented to work on PostgreSQL or PostgreSQL-like databases.
#'
#' Try to connect or try to connect as "user".  If the user (or its support structure) does not exists, then create the support structure and the "user".
#'
#' @param driver String. Defaults to getOption("econmodel_db_driver"). String.  Default is "PostgreSQL".  Currently only an implementation exists using PostgreSQL and PostgreSQL-like databases.
#' @param conn Maybe a PostgreSQLConnection.  Its validility and active status is checked (and re-used if valid and active).
#' @param String.  Contains the name of the variable that contains the name of the "connection" in the environment "env".
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
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @returns 1. invisibly a DBI connection 2. Object named "connEM" is created, connected, and assigned to the environment "env".
#' @examples
#' \dontrun{
#' dbLoginEM()
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbConnect
#' @export
dbLoginEM <- function(driver, conn, connName, user, password = user, host, dbname = user, port,
                      tty, options, forceISOdate, env, display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  if(missing(connName)) {
    connName <- "connEM"
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

  if(!missing(conn)) {
    Results <- dbIsConnectedEM(conn, display = display, exec = exec)
    if(exec) {
      if(unlist(Results)) {
        if(unlist(dbIsConnectedEM(get(connName, envir = env, inherits = FALSE), display = display, exec = exec))) {
          dbDisconnectEM(connName = connName, env = env)
        }
        # re-use
        assign(connName, conn, envir = env)
        return(invisible(conn))
      }

    }
  }

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


  # (survey of the) current connection, if any

  connNameExists  <- exists(connName, envir = env, inherits = FALSE)
  connNameConnected <- FALSE
  if(connNameExists) {
    connNameConnected <- dbIsConnectedEM(get(connName, envir = env, inherits = FALSE), display = display, exec = exec)
  }
  if(connNameConnected) {
    CurrentUser <- dbGetCurrentuserEM(get(connName, envir = env, inherits = FALSE), display = display, exec = exec)
    if(exec) {
      if(unlist(CurrentUser) == user) {
        if(!unlist(dbExistsSchemaEM(get(connName, envir = env, inherits = FALSE), schema = user, display = display, exec = exec)))
          dbCreateSchemaEM(get(connName, envir = env, inherits = FALSE), schema = user, display = display, exec = exec)
        return(invisible(get(connName, envir = env, inherits = FALSE)))
      }
    }
  }

  # try to find or create a connection . . .

  drv <- try({drv <- DBI::dbDriver(getOption("econmodel_db_driver"))}, silent = TRUE)
  if(inherits(drv, "try-error")) {
    message("\"Driver\" failed to load.")
    if(exec) {
      # NOTHING CAN BE DONE
      stop("\"Driver\" failed to load.")
    }
  }

  SessionConn <- try({DBI::dbConnect(drv,
                                     user = user, password = password,
                                     host = host, dbname = dbname, port = port,
                                     tty = tty, options = dboptions, forceISOdate = forceISOdate)},
                     silent = TRUE)
  if(inherits(SessionConn, "try-error")) {
    SessionConnLoginWorks <- FALSE
    message("User login failed.")
  } else {
    SessionConnLoginWorks <- TRUE
    message("User login succeeded.")
    if(exec) {
      if(!unlist(dbExistsSchemaEM(SessionConn, schema = user, display = display, exec = exec)))
                 dbCreateSchemaEM(SessionConn, schema = user, display = display, exec = exec)
      assign(connName, SessionConn, envir = env)
      return(invisible(SessionConn))
    }
  }

  PostgresConn <- try({DBI::dbConnect(drv,
                                      user = "postgres", password = "postgres",
                                      host = host, dbname = "postgres", port = port,
                                      tty = tty, options = dboptions, forceISOdate = forceISOdate)},
                      silent = TRUE)
  if(inherits(conn, "try-error")) {
    PostgresConnLoginWorks <- FALSE
    message("Postgres login failed.")
  } else {
    PostgresConnLoginWorks <- TRUE
    message("Postgres login succeeded.")
    if(exec) {
      # dispatcher database
      if(!unlist(dbExistsUserEM(PostgresConn, user   = "r_user_econmodel", display = display, exec = exec)))
                 dbCreateUserEM(PostgresConn, user = "r_user_econmodel", attributes = c("LOGIN", "CREATEDB", "CREATEROLE"), display = display, exec = exec)
      if(!unlist(dbExistsDbaseEM(PostgresConn, dbname = "r_user_econmodel", display = display, exec = exec)))
                 dbCreateDbaseEM(PostgresConn, dbname = "r_user_econmodel", display = display, exec = exec)

      dbDisconnectEM(PostgresConn)
    }

  }
  DispatcherConn <- try({DBI::dbConnect(drv,
                                        user = "r_user_econmodel", password = "r_user_econmodel",
                                        host = host, dbname = "r_user_econmodel", port = port,
                                        tty = tty, options = dboptions, forceISOdate = forceISOdate)},
                        silent = TRUE)
  if(inherits(conn, "try-error")) {
    DispatcherConnLoginWorks <- FALSE
    message("Dipatcher login failed.")
  } else {
    DispatcherConnLoginWorks <- TRUE
    message("Dipatcher login succeeded.")
    if(exec) {
      if(!unlist(dbExistsSchemaEM(DispatcherConn, schema = "r_user_econmodel", display = display, exec = exec)))
                dbCreateSchemaEM(DispatcherConn, schema = "r_user_econmodel", display = display, exec = exec)
      # user database
      if(!unlist(dbExistsUserEM(DispatcherConn, user = user, display = display, exec = exec)))
                 dbCreateUserEM(DispatcherConn, user = user, display = display, exec = exec)
      if(!unlist(dbExistsDbaseEM(DispatcherConn, dbname = user, display = display, exec = exec)))
                 dbCreateDbaseEM(DispatcherConn, dbname = user, display = display, exec = exec)
      dbDisconnectEM(DispatcherConn)
      dbLoginEM(user = user)
    }
  }

  return(invisible(data.frame(DBLOGINEM = logical())))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}





#' Executes in the  database
#'
#' Disconnects the R object passed "conn".
#' Alternately, can disconnect and remove the DBI connection object connName (default is "connEM") from the environment "env".
#' The function will just do one or the other, but not both.  Does NOT remove the passed R object "conn".
#'
#' @param conn PostgreSQL DBI connection. Optional. This may be "passed" instead of  "connName".
#' @param Statement String. Required.  DML/DDL/DCL to execute
#' @param time_zone Execution time zone. Default is "UTC".
#' @param client_encoding Execution encoding. Default is "UTF8".
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @returns Execution of "conn" from the database or disconnects "connName" from the database and removes it from the environment "env".
#' @examples
#' \dontrun{
#' dbExecuteEM(get("connEM"), Statement = "CREATE TABLE xyz();")
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbExecuteEM <- function(conn, Statement, time_zone = "UTC", client_encoding = "UTF8", display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  if(missing(conn)) {
    stop("Parameter \"conn\" must be provided.")
  }

  if(missing(Statement)) {
    stop("Parameter \"Statement\" must be provided.")
  }

  oldtz <- Sys.getenv("TZ")
  Sys.setenv(TZ=time_zone)
  on.exit({Sys.setenv(TZ=oldtz)})

  DBI::dbExecute(conn, statement =  paste0("SET client_encoding TO '", client_encoding, "';"))

  ## Build the query
  tmp.query <- Statement

  ## Display the query
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }

  Results <- try({DBI::dbExecute(conn, statement = tmp.query)}, silent = T)
  if(exec && !inherits(Results, "try-error")) {
    return(data.frame(DBEXECUTEEM = TRUE))
  } else if(!exec) {
    return(invisible(data.frame(DBEXECUTEEM = logical())))
  } else {
    message(paste0("Statement failed: ", tmp.query))
    return(data.frame(DBEXECUTEEM = FALSE))
  }

  return(invisible(data.frame(DBEXECUTEEM = logical())))

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
#' @returns Disconnects "conn" from the database or disconnects "connName" from the database and removes it from the environment "env".
#' @examples
#' \dontrun{
#' dbDisconnectEM() # default is connection variable "connEM" in the .GlobalEnv
#' dbDisconnectEM(conn)
#' dbDisconnectEM(connName = "connEM", env = .Globalenv)
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbDisconnectEM <- function(conn, connName, env) {
tryCatchLog::tryCatchLog({

  # missing(conn) # is handled below

  if(missing(connName)) {
    connName <- "connEM"
  }

  if(missing(env)) {
    env <- .GlobalEnv
  }

  TriedDisconnectedOne <- FALSE
  if(!missing(conn)) {
    try(DBI::dbDisconnect(conn), silent = T)
    message(paste0("Tried to disconnect the passed R object \"conn\"."))
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
    message(paste0("Tried to disconnect the R object \"", connName, "\" and remove it\nfrom the environment ", capture.output(env), "."))
  }

  # DBI::dbDisconnect
  # always returns TRUE
  return(invisible(data.frame(DBDISCONNECTEM = TRUE)))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


#' Disconnect from the econModel database
#'
#  Disconnect and remove the DBI connection object connName (default is "connEM") from the environment "env".
#'
#' @param connName String.  Name of the database connection object. Optional. This may be "passed" instead of  "conn".
#' @param env Environment. Default is the global environment .GlobalEnv.  Location of the connection object "connName".
#' @returns Disconnects "connEM" from the database or disconnects "connName" from the database and removes it from the environment "env".
#' @examples
#' \dontrun{
#' dbLogoutEM() # default is connection variable "connEM" in the .GlobalEnv
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbLogoutEM <- function(connName, env) {
tryCatchLog::tryCatchLog({

  if(missing(connName)) {
    connName <- "connEM"
  }

  if(missing(env)) {
    env <- .GlobalEnv
  }

  Results <- dbDisconnectEM(connName = connName, env = env)
  if(exec && !inherits(Results, "try-error")) {
    return(data.frame(DBLOGOUTEM = unlist(Results)))
  } else if(!exec) {
    return(invisible(data.frame(DBLOGOUTEM = logical())))
  } else {
    message(paste0("Statement failed: ", tmp.query))
    return(data.frame(DBLOGOUTEM = FALSE))
  }

  return(invisible(data.frame(DBLOGOUTEM = logical())))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}





#' User Existence?
#'
#' Determine if a user exists in the database.
#'
#' @param conn PostgreSQL DBI connection. Required.
#' @param user String. Required.  Potential user in the database.
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @returns TRUE(exists) or FALSE(not exists)
#' @examples
#' \dontrun{
#'  dbExistsUserEM(conn, user = "r_user_econmodel")
#'  dbExistsUserEM(conn, user = "rtmp")
#' }
#' @importFrom tryCatchLog tryCatchLog
dbExistsUserEM <- function(conn, user, display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  if(missing(conn)) {
    stop("Parameter \"conn\" is required.")
  }

  if(missing(user)) {
    stop("Parameter \"user\" is required.")
  }

  tmp.query <- paste0("SELECT EXISTS(SELECT usename FROM pg_catalog.pg_user WHERE usename = ", DBI::dbQuoteLiteral(conn, x = user), ");")

  Results <- dbGetQueryEM(conn, Statement = tmp.query, display = display, exec = exec)
  if(exec && NROW(Results)) {
   return(data.frame(DBEXISTSUSEREM = unlist(Results)))
  } else if(!exec) {
    return(invisible(data.frame(DBEXISTSUSEREM = logical())))
  } else {
    return(data.frame(DBEXISTSUSEREM = FALSE))
  }

  return(invisible(data.frame(DBEXISTSUSEREM = logical())))

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
#' @returns TRUE(success) or Error(failure)
#' @examples
#' \dontrun{
#' dbCreateUserEM(conn, user = "r_user_econmodel", attributes = c("LOGIN", "CREATEDB", "CREATEROLE"))
#' dbCreateUserEM(conn, user = "rtmp")
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbExecute dbQuoteLiteral
dbCreateUserEM <- function(conn, user, attributes = c("LOGIN"), password = user, display = TRUE, exec = TRUE) {
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

  password <- paste0("password ", DBI::dbQuoteLiteral(conn, x = password))

  tmp.query <- paste0("CREATE ROLE ", user, " ", attributes, " NOINHERIT ", password, ";")
  Results <- try({dbExecuteEM(conn, Statement = tmp.query, display = display, exec = exec)})
  if(exec && !inherits(Results, "try-error")) {
    return(data.frame(DBCREATEUSEREM = unlist(Results)))
  } else if(!exec) {
    return(invisible(data.frame(DBCREATEUSEREM = logical())))
  } else {
    message(paste0("Statement failed: ", tmp.query))
    return(data.frame(DBCREATEUSEREM = FALSE))
  }

  return(invisible(data.frame(DBCREATEUSEREM = logical())))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Schema Existence?
#'
#' Determine if a schema exists in the database.
#'
#' @param conn PostgreSQL DBI connection. Required.
#' @param schema String. Required.  Potential schema in the database.
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @returns TRUE(exists) or FALSE(not exists)
#' @examples
#' \dontrun{
#'  dbExistsSchemaEM(conn, schema = "r_user_econmodel")
#'  dbExistsSchemaEM(conn, schema = "rtmp")
#' }
#' @importFrom tryCatchLog tryCatchLog
dbExistsSchemaEM <- function(conn, schema, display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  if(missing(conn)) {
    stop("Parameter \"conn\" is required.")
  }

  if(missing(schema)) {
    stop("Parameter \"schema\" is required.")
  }

  tmp.query <-  paste0("SELECT EXISTS(SELECT 1 FROM pg_catalog.pg_namespace WHERE nspname = ", DBI::dbQuoteLiteral(conn, x = schema), ");")

  Results <- dbGetQueryEM(conn, Statement = tmp.query, display = display, exec = exec)
  if(exec && NROW(Results)) {
    return(data.frame(DBEXISTSSCHEMAEM = unlist(Results)))
  } else if(!exec) {
    return(invisible(data.frame(DBEXISTSSCHEMAEM = logical())))
  } else {
    return(data.frame(DBEXISTSSCHEMAEM = FALSE))
  }

  return(invisible(data.frame(DBEXISTSSCHEMAEM = logical())))

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
#' @returns TRUE(success) or Error(failure)
#' @examples
#' \dontrun{
#'  dbCreateSchemaEM(conn, schema = "r_user_econmodel")
#'  dbCreateSchemaEM(conn, schema = "rtmp")
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbExecute dbQuoteLiteral
dbCreateSchemaEM <- function(conn, schema, role_specification = schema, grant_all_roles = schema, display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  if(missing(conn)) {
    stop("Parameter \"conn\" is required.")
  }

  if(missing(schema)) {
    stop("Parameter \"schema\" is required.")
  }

  if(1L < NROW(role_specification)) {
    stop("Parameter \"role_specification\" can only have one role.")
  }

  tmp.query <- paste0("CREATE SCHEMA ", schema, " AUTHORIZATION ", role_specification, ";")
  # Execute the query
  Results <- try({dbExecuteEM(conn, Statement = tmp.query, display = display, exec = exec)})
  if(exec && !inherits(Results, "try-error")) {
    # return(data.frame(DBCREATESCHEMAEM = unlist(Results)))
  } else if(!exec) {
    # return(invisible(logical()))
  } else {
    message(paste0("Statement failed: ", tmp.query))
    return(data.frame(DBCREATESCHEMAEM = FALSE))
  }

  lapply(grant_all_roles, function(grant_all_role) {

    tmp.query <- paste0("GRANT ALL ON SCHEMA ", schema, " TO ", grant_all_role, ";")
    # Execute the query
    Results <- try({dbExecuteEM(conn, Statement = tmp.query, display = display, exec = exec)})
    if(exec && !inherits(Results, "try-error")) {
      # return(data.frame(DBCREATESCHEMAEM = unlist(Results)))
    } else if(!exec) {
      # return(invisible(logical()))
    } else {
      message(paste0("Statement failed: ", tmp.query))
      return(data.frame(DBCREATESCHEMAEM = FALSE))
    }

  })

  if(exec) {
    return(data.frame(DBCREATESCHEMAEM = TRUE))
  }
  if(display) {
    return(invisible(data.frame(DBCREATESCHEMAEM = logical())))
  }

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Database Existence?
#'
#' Determine if a database exists in the cluster.
#'
#' @param conn PostgreSQL DBI connection. Required.
#' @param dbname String. Required.  Potential database in the cluster.
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @returns TRUE(exists) or FALSE(not exists)
#' @examples
#' \dontrun{
#'  dbExistsDbaseEM(conn, dbname = "r_user_econmodel")
#'  dbExistsDbaseEM(conn, dbname = "rtmp")
#' }
#' @importFrom tryCatchLog tryCatchLog
dbExistsDbaseEM <- function(conn, dbname, display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  if(missing(conn)) {
    stop("Parameter \"conn\" is required.")
  }
  if(missing(dbname)) {
    stop("Parameter \"dbname\" is required.")
  }


  tmp.query <- paste0("SELECT EXISTS(SELECT 1 FROM pg_catalog.pg_database WHERE datname = ", DBI::dbQuoteLiteral(conn, x = dbname), ");")
  Results <- dbGetQueryEM(conn, Statement = tmp.query, display = display, exec = exec)
  if(exec && NROW(Results)) {
    return(data.frame(DBEXISTSDBASEEM = unlist(Results)))
  } else if(!exec) {
    return(invisible(data.frame(DBEXISTSDBASEEM = logical())))
  } else {
    return(data.frame(DBEXISTSDBASEEM = FALSE))
  }

  return(invisible(data.frame(DBEXISTSDBASEEM = logical())))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Format input for database schema/object names.
#'
#' This is a near copy of the R CRAN package rpostgis function dbTableNameFix.
#'
#' Internal rpostgis function to return common (length = 2) schema
#' and table name vector from various table and schema + table name
#' inputs.
#'
#' @param conn A connection object. Must be provided but can be set NULL,
#' where a dummy connection will be used.
#' @param o.nm Object name string, length 1-2.
#' @param as.identifier Boolean whether to return (schema,table) name as database
#' sanitized identifiers (TRUE) or as regular character (FALSE)
#' @param dbQuote String. Only used when "as.identifier = TRUE". Default is "Identifier". Alternately, this value can be "Literal."
#' @return character vector of length 2. Each character element is in
#'     (escaped) double-quotes when as.identifier = TRUE.
#' @param display	Logical. Whether to display the query (defaults to TRUE).
#' @param exec	Logical. Whether to execute the query (defaults to TRUE).
#' @keywords internal
#' @importFrom DBI dbQuoteIdentifier dbQuoteLiteral
#' @importFrom DBI dbQuoteString
#' @examples
#' \dontrun{
#' name <- c("schema","table")
#' dbObjectNameFixEM(conn, name)
#'
#' #current search path schema is added to single-length character object (if only table is given)
#' name<-"table"
#' dbObjectNameFixEM(conn,name)
#'
#' #schema or table names with double quotes should be given exactly as they are
#' (make sure to wrap in single quotes in R):
#' name <- c('sch"ema','"table"')
#' dbObjectNameFixEM(conn,name)
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbQuoteIdentifier ANSI
dbObjectNameFixEM <- function(conn, o.nm, as.identifier = TRUE, dbQuote = "Identifier", display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  if(missing(conn)) {
    stop("Parameter \"conn\" must be provided.")
  }

  # case of no schema provided
  if (length(o.nm) == 1 && !is.null(conn) && !inherits(conn, what = "AnsiConnection")) {
    schemalist <- dbGetQueryEM(conn, Statement = "SELECT nspname AS s FROM pg_catalog.pg_namespace;", display = display, exec = exec)$S
    user <- dbGetQueryEM(conn, Statement = "SELECT CURRENT_USER AS user;", display = display, exec = exec)$USER
    schema <- dbGetQueryEM(conn, Statement = "SHOW SEARCH_PATH;", display = display, exec = exec)$SEARCH_PATH
    schema <- gsub(" ","",unlist(strsplit(schema,",",fixed=TRUE)),fixed=TRUE)
    # use user schema if available
    if ("\"$user\"" == schema[1] && user %in% schemalist) {
      sch <- user
    } else {
      sch <- schema[!schema=="\"$user\""][1]
    }
    o.nm <- c(sch, o.nm)
  }
  if (length(o.nm) > 2) {
    stop("Invalid PostgreSQL table/view name. Must be provided as one ('table') or two-length c('schema','table') character vector.")
  }
  if (is.null(conn)) {conn<-DBI::ANSI()}
  if (!as.identifier) {return(o.nm)} else {
    if (dbQuote == "Identifier") {
      o.nm <- DBI::dbQuoteIdentifier(conn, o.nm)
    }
    if (dbQuote == "Literal") {
      o.nm <- DBI::dbQuoteLiteral(conn, o.nm)
    }
    return(o.nm)
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
#' @returns TRUE(success) or Error(failure)
#' @examples
#' \dontrun{
#' dbCreateDbaseEM(conn, dbname = "r_user_econmodel")
#' dbCreateDbaseEM(conn, dbname = "rtmp")
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbExecute dbQuoteLiteral
dbCreateDbaseEM <- function(conn, dbname, owner = dbname, display = TRUE, exec = TRUE) {
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

  tmp.query <- "SELECT CURRENT_USER;"
  Results <- dbGetQueryEM(conn, Statement = tmp.query, display = display, exec = exec)
  if(exec && NROW(Results)) {
    CurrentUser <- Results
    # return(data.frame(DBCREATEDBASEEM = unlist(Result)))
  } else if(!exec) {
    # return(invisible(logical()))
  } else {
    message(paste0("Statement failed: ", tmp.query))
    return(data.frame(DBCREATEDBASEEM = FALSE))
  }

  tmp.query <- paste0("GRANT ", owner, " TO ", unlist(CurrentUser), ";")
  Results <- try({dbExecuteEM(conn, Statement = tmp.query, display = display, exec = exec)})
  if(exec && !inherits(Results, "try-error")) {
    # return(data.frame(DBCREATEDBASEEM = unlist(Results)))
  } else if(!exec) {
    # return(invisible(data.frame(DBCREATEDBASEEM = logical())))
  } else {
    message(paste0("Statement failed: ", tmp.query))
    return(data.frame(DBCREATEDBASEEM = FALSE))
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
  Results <- try({dbExecuteEM(conn, Statement = tmp.query, display = display, exec = exec)})
  if(exec && !inherits(Results, "try-error")) {
    # return(data.frame(DBCREATEDBASEEM = unlist(Results)))
  } else if(!exec) {
    # return(invisible(data.frame(DBCREATEDBASEEM = logical())))
  } else {
    message(paste0("Statement failed: ", tmp.query))
    return(data.frame(DBCREATEDBASEEM = FALSE))
  }

  tmp.query <- paste0("ALTER DATABASE ", dbname, " SET TIME ZONE 'UTC';", display = display, exec = exec)
  Results <- try({dbExecuteEM(conn, Statement = tmp.query, display = display, exec = exec)})
  if(exec && !inherits(Results, "try-error")) {
    # return(data.frame(DBCREATEDBASEEM = unlist(Results)))
  } else if(!exec) {
    # return(invisible(data.frame(DBCREATEDBASEEM = logical())))
  } else {
    message(paste0("Statement failed: ", tmp.query))
    return(data.frame(DBCREATEDBASEEM = FALSE))
  }

  if(exec) {
    return(data.frame(DBCREATEDBASEEM = TRUE))
  }
  if(display) {
    return(invisible(data.frame(DBCREATEDBASEEM = logical())))
  }

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' From the Cluster, Remove Old Work Databases
#'
#' In the cluster, logs on to the database "r_user_econmodel" as user "rtmp%" and (tries to) drop the "rtmp%" database.
#' Does not drop the "current work database": "getOption("econmodel_db_dbname")".
#'
#' Note, is also attempt to drop the Old User.
#'
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @returns TRUE(always).  Will attempt to drop each 'rtmp%' database (but not the current work database).
#' @examples
#' \dontrun{
#' clRemoveOldWorkDbasesEM()
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbDriver dbConnect dbQuoteLiteral dbDisconnect
#' @export
clRemoveOldWorkDbasesEM <- function(display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  ops <- options()
  options(warn = 1L)

  drv  <- DBI::dbDriver("PostgreSQL")
  conn <- DBI::dbConnect(drv, user = "r_user_econmodel", password = "r_user_econmodel", dbname = "r_user_econmodel")
  NotDatabase <- DBI::dbQuoteLiteral(conn, x = getOption("econmodel_db_dbname"))

  tmp.query <-  paste0("SELECT datname FROM pg_catalog.pg_database WHERE datname LIKE 'rtmp%' AND datname != ", NotDatabase, ";")
  Databases <- dbGetQueryEM(conn, Statement = tmp.query, display = TRUE, exec = TRUE)
  Databases  <- unlist(Databases)
  dbDisconnectEM(conn)

  AllResults <- logical()

  lapply(Databases, function(dbname) {
    try({
      # I can not be "in" the database I am trying to drop
      # Avoid the error: ERROR:  cannot drop the currently open database
      conn <- DBI::dbConnect(drv, user = dbname, password = dbname, dbname = "r_user_econmodel")
      # trying to drop my own database
      tmp.query <- paste0("DROP DATABASE ", dbname, ";")
      Results <- dbExecuteEM(conn, Statement = tmp.query, display = display, exec = exec)
      AllResults <<- c(AllResults, unlist(Results))
      dbDisconnectEM(conn)

    }, silent = TRUE)
    invisible()
  })

  conn <- DBI::dbConnect(drv, user = "r_user_econmodel", password = "r_user_econmodel", dbname = "r_user_econmodel")
  lapply(Databases, function(dbname) {
    try({
      # trying to drop those users (with the same name as the database)
      tmp.query <- paste0("DROP USER ", dbname, ";")
      Results <- dbExecuteEM(conn, Statement = tmp.query, display = display, exec = exec)
      AllResults <<- c(AllResults, unlist(Results))
    }, silent = TRUE)
    invisible()
  })
  dbDisconnectEM(conn)

  options(ops)

  if(exec) {
    if(all(AllResults)) {
      return(data.frame(DBREMOVEOLDWORKDBASESEM = TRUE))
    } else {
      return(data.frame(DBREMOVEOLDWORKDBASESEM = FALSE))
    }
  }
  if(display) {
    return(invisible(data.frame(DBREMOVEOLDWORKDBASESEM = logical())))
  }

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}






#' Of a PostgreSQL Object, Get Inherited or Inherit_From Objects
#'
#' Of the PostgreSQL object, get the child/parent objects (if any) with the schema.
#'
#' Of column results named *_RELKIND, see the table pg_namespace column relkind.
#' These values are the following: r = ordinary table, i = index, S = sequence, t = TOAST table, v = view, m = materialized view, c = composite type, f = foreign table, p = partitioned table, I = partitioned index.
#'
#' Of column results named like '_PART_KEY_DEF' and '_PART_BOUND', these are the partition object key (LISTs, RANGEs, and HASHs) and the partitions (child objects) bounds (FOR VALUES . . .) (of the partition object).  See the references.
#'
#' @param conn PostgreSQLConnection . Required.
#' @param name String. Required. Name of the object.
#' @param side String. View from the "parent side. Default is "parent".  Alternately, the view from the "child side" is the value "child".
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @returns Dataframe of inherited or inherit_from "objects" (if any) (with schema name).
#' @references
#' \cite{Get number of partitions in PostgreSQL database
#' \url{https://stackoverflow.com/questions/8634144/get-number-of-partitions-in-postgresql-database }
#' }
#' @references
#' \cite{How to identify the ranges over which a postgres table was partitioned?
#' \url{https://dba.stackexchange.com/questions/221277/how-to-identify-the-ranges-over-which-a-postgres-table-was-partitioned}
#' }
#' @examples
#' \dontrun{
#' dbListInheritEM(conn, name = "sample")
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbListInheritEM <- function(conn, name, side = "parent", display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  if(missing(conn)) {
    stop("Parameter \"conn\" is required.")
  }

  if(missing(name)) {
    stop("Parameter \"name\" is required.")
  }

  SchemaAndName <- dbObjectNameFixEM(conn, o.nm = name, as.identifier = TRUE, dbQuote = "Literal", display = display, exec = exec)

  if(side == "parent") {
    Restriction <-
      paste0("
        parent_schema = ", first(SchemaAndName), "
    AND        parent = ", last(SchemaAndName)
      )
  }
  if(side == "child") {
    Restriction <-
      paste0("
         child_schema = ", first(SchemaAndName), "
     AND child        = ", last(SchemaAndName)
      )
  }

  Statement <- paste0("
SELECT q.* FROM (
  SELECT
      nmsp_parent.nspname::text AS parent_schema,
      parent.relname::text      AS parent,
      parent.relkind::text      AS parent_relkind,
            pg_catalog.pg_get_expr(parent.relpartbound, parent.oid) AS parent_part_bound,
      pg_catalog.pg_get_partkeydef(parent.oid) AS parent_part_key_def,
      pg_inherits.inhseqno,
      nmsp_child.nspname::text  AS child_schema,
      child.relname::text       AS child,
      child.relkind::text       AS child_relkind,
      pg_catalog.pg_get_expr(child.relpartbound, child.oid) AS child_part_bound,
      pg_catalog.pg_get_partkeydef(child.oid) AS child_part_key_def
  FROM pg_inherits
  FULL JOIN pg_class parent        ON pg_inherits.inhparent = parent.oid
  FULL JOIN pg_class child         ON pg_inherits.inhrelid = child.oid
  FULL JOIN pg_namespace nmsp_parent   ON nmsp_parent.oid  = parent.relnamespace
  FULL JOIN pg_namespace nmsp_child    ON nmsp_child.oid   = child.relnamespace
) q
WHERE", Restriction, ";")

  Results <- dbGetQueryEM(conn, Statement = Statement, display = display, exec = exec)

  if(NROW(Results)) {
    return(Results)
  } else {
    return(data.frame(list(PARENT_SCHEMA = "", PARENT = "", PARENT_RELKIND = "", PARENT_PART_KEY_DEF = "", PARENT_PART_BOUND = "", INHSEQNO = 0L, CHILD_SCHEMA = "", CHILD = "", CHILD_REL_KIND = "", CHILD_PART_KEY_DEF = "", CHILD_PART_BOUND = ""))[FALSE, , drop = F])
  }

  return(invisible(data.frame(DBLISTINHERITEM = logical())))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Create a Named Vector from a dataframe, Table or Vector
#'
#' @description
#' \preformatted{
#' see ? caroline::nv
#'
#' This is R cran package caroline function "nv" with modifications
#' JAN 2021
#' https://github.com/cran/caroline/blob/af201137e4a31d675849291a1c9c07a0933b85de/R/convert.R
#'
#' Modifications:
#'
#' * function "names" is replaced with the safer function "Names"
#'   Note, keeping function "Names" and not replacing using "colnames"
#'   keeps generalization ??
#' }
#' @param x	The source dataframe, table, vector, or factor
#' @param name The column name you would like to pull out as a named vector. OR the names of the vector (if x is a vector)
#' @returns a named vector or factor
#' @importFrom tryCatchLog tryCatchLog
#' @export
nameVect <- function(x, name){
tryCatchLog::tryCatchLog({

  if(class(x)=='data.frame'){
    v <- x[,name[1]]
    if(length(name)==2)
      Names(v) <- x[,name[2]]
    else
      Names(v) <- rownames(x)
  }else{
    if(NCOL(x)!=1)
      stop('x must be unidimentional')
    v <- x
    Names(v) <- name
  }
  v
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}




#' Create a PostgreSQL Partitioned and/or Bounded Table
#'
#' Exposes an interface to simple PostgreSQL
#' `CREATE TABLE . . . PARTITION BY . . . PARTITION OF . . . FOR VALUES . . . ` commands.
#'
#' This code is partially inspired by R CRAN package DBI S4 methods sqlCreateTable and dbCreateTable.
#'
#' NOTE: In PostgreSQL, one can not convert a table to a partitioned table and vice-versa.
#'
#' @param conn A PostgreSQL database connection.
#' @param temporary Logical. Create a temporary table.  Default is FALSE.
#' @param if.not.exists Logical. Default is FALSE. If TRUE, add the " IF NOT EXISTS " clause.
#' @param name Name of the table. Escaped with.
#' @param like.name  Create a table like this one. If this has a name, then all other parameters are ignored EXCEPT "like.name.defaults" and "like.name.constraints".
#' @param like.name.defaults Logical.  Default is FALSE. This is an option only when "like.name" has a value.
#' @param like.name.constraints Logical. Default is FALSE. This is an option only when "like.name" has a value.
#'   [DBI::dbQuoteIdentifier()].
#' @param fields Either a character vector or a data frame.
#'
#'   A named character vector: Names are column names, values are types.
#'   Names are escaped with [DBI::dbQuoteIdentifier()].
#'   Field types are unescaped.
#'
#'   A data frame: field types are generated using
#'   [DBI::dbDataType()].
#'
#' @param temporary If `TRUE`, will generate a temporary table statement.
#' @param part.by String. `PARTITION OF parent_table`
#' @param part.bound String. `[ ({ column_name [ WITH OPTIONS ] [ column_constraint [ ... ] ] | table_constraint } [, ... ] ) ] { FOR VALUES partition_bound_spec | DEFAULT }`.
#' @param part.key.def String. `[ PARTITION BY { RANGE | LIST | HASH } ( { column_name | ( expression ) } [ COLLATE collation ] [ opclass ] [, ... ] ) ]`.
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), user="postgres", password="postgres", dbname="postgres")
#' dbCreatePartBoundTableEM(conn, "mtcars", mtcars, part.key.def = "LIST(CAST(gear AS INTEGER))")
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbDataType dbQuoteIdentifier
#' @export
dbCreatePartBoundTableEM <- function(conn, name, temporary = FALSE, if.not.exists = FALSE, like.name = character() , like.name.defaults = FALSE, like.name.constraints = FALSE,  fields, part.by = character(), part.bound = character(), part.key.def = character(), display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  table <- dbObjectNameFixEM(conn, o.nm = name, display = display, exec = exec)
  if(temporary) {
    if(length(table) == 2) {
      tableque <- paste0(last(table))
    } else {
      tableque <- paste0(table)
    }
  } else {
    tableque <- paste(table, collapse = ".")
  }

  like.tableque <- character()
  if(length(like.name)) {
    like.table <- dbObjectNameFixEM(conn, o.nm = like.name, display = display, exec = exec)
    if(temporary) {
      if(length(table) == 2) {
        like.tableque <- paste0(last(like.table))
      } else {
        like.tableque <- paste0(like.table)
      }
    } else {
      like.tableque <- paste(like.table, collapse = ".")
    }
  }

  if(length(like.name)) {

    tmp.query <- paste0("CREATE TABLE ", if (temporary) "TEMPORARY ", if(if.not.exists) " IF NOT EXISTS ",
                        like.tableque, " (", " LIKE ", tableque,
                        if(like.name.defaults)    " INCLUDING DEFAULTS ",
                        if(like.name.constraints) " INCLUDING CONSTRAINTS ",
                         ");")
    dbExecuteEM(conn, Statement = tmp.query, display = display, exec = exec)
    # short circuit
    return(invisible(TRUE))

  }

  if(is.data.frame(fields)) {
    fields <- vapply(fields, function(x) DBI::dbDataType(conn, x), character(1))
  }

  field_names <- DBI::dbQuoteIdentifier(conn, names(fields))
  field_types <- unname(fields)
  fields <- paste0(field_names, " ", field_types)

  if(length(part.by)) {
    part.by <- paste0(" PARTITION OF ", part.by, " ")
  }

  if(length(part.bound)) {
    part.bound <- paste0(" ", part.by, " ", part.bound, " ")
  }

  if(length(part.key.def)) {
    part.key.def <- paste0(" PARTITION BY ", part.key.def, " ")
  }

  tmp.query <- paste0(
    "CREATE ", if (temporary) "TEMPORARY ", "TABLE ", if(if.not.exists) " IF NOT EXISTS ",
    tableque, " (\n",
    "  ", paste(fields, collapse = ",\n  "), " \n)", part.by, part.bound, part.key.def, "\n"
  )

  Results <- dbExecuteEM(conn, Statement = tmp.query, display = display, exec = exec)
  return(data.frame(DBCREATEPARTBOUNDTABLEEM = unlist(Results)))

  return(invisible(data.frame(DBCREATEPARTBOUNDTABLEEM = logical())))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' From a PostgreSQL Object Get Its Partition Key Definition
#'
#' Of a PostgreSQL  partition table, detect its partition key definition.
#' If not a "partition table", an empty character vector is returned.
#'
#' This is only designed to work on List partitioned tables with the non-expression key.
#' Will work: "PARTITION BY LIST (c1)" is stored as "LIST (c1)".
#' Will not work: "PARTITION BY LIST (CAST(c1) AS INTEGER)" is stored as "LIST (((c1)::integer))".
#'
#' @param conn PostgreSQLConnection.
#' @param DfName String. Name of the PostgreSQL object.
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @returns vector of size 1 of the partition key definition.
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbPartKeyColEM <- function(conn, DfName, display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({
  DetectedPartKeyDef <- character()
  Results <- dbListInheritEM(conn, DfName)
  if(!NROW(Results)) {
    stop(paste0("Object ", DfName, " is missing from the database."))
  }
  SubResults <- dbListInheritEM(conn, name = DfName, display = display, exec = exec)$PARENT_PART_KEY_DEF
  if(is.na(SubResults)) {
    # not a "p" - partitioned. (is "r" - regular)
    DetectedPartKeyCol <- character()
  } else {
    # "p" - partitioned
    # SIMPLE COLUMN NAMES ONLY (a)
    # NOT TOO CLEVER - WILL NOT CORRECTLY EXTRACT EXPRESSIONS
    DetectedPartKeyCol <- RegExtract("(?<=\\()(\\w+)(?=\\))", SubResults)
  }
  return(DetectedPartKeyCol)

  return(invisible(data.frame(DBPARTKEYCOLEM = logical())))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' From a PostgreSQL Object Get Its Partition Bounds
#'
#' Of a PostgreSQL inheriting partition, detect its partition bounds.
#' If not a "inheriting partition", an empty character vector is returned.
#'
#' This is only designed to work on List paritions the non-expression partition bounds.
#'
#' @param conn PostgreSQLConnection.
#' @param DfName String. Name of the PostgreSQL object.
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @returns vector of partiton bound list values.
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbPartBoundEM <- function(conn, DfName, display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({
  DetectedPartBound <- character()
  Results <- dbListInheritEM(conn, DfName)
  if(!NROW(Results)) {
    stop(paste0("Object ", DfName, " is missing from the database."))
  }
  SubResults <- dbListInheritEM(conn, name = DfName, display = display, exec = exec)$PARENT_PART_BOUND
  if(is.na(SubResults)) {
    # does not have a parent
    DetectedPartBound <- character()
  } else {
    # does have a parent
    # SIMPLE COLUMN VALUES ONLY (a,b)
    # NOT TOO CLEVER - WILL NOT CORRECTLY EXTRACT EXPRESSIONS
    PartBound <- RegExtract("(?<=\\()(\\w+)(?=\\))", SubResults)
    PartBound <- gsub(" ", "", PartBound)
    PartBound <- unlist(strsplit(PartBound, ","))
    DetectedPartBound <- PartBound
  }
  return(DetectedPartBound)

  return(invisible(data.frame(DBPARTBOUNDTABLEEM = logical())))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Server Side Table Column Stored Type C Classes
#'
#' Get the internal table of database server columns.
#'
#' @details
#' \preformatted{
#'
#' Typical (SQLite) columns:
#' NAME(chr)  TYPE(chr)
#'
#' Typical SQLite column TYPE values:
#' TYPE: character double integer
#'
#' Typical PostgreSQL  columns:
#' NAME(chr), SCLASS(chr), TYPE(chr),
#' LEN(int), PRECISION(int), SCALE(int) NULLOK(logi)
#'
#' Typical PostgreSQL column values:
#' SCLASS: character double integer logical Date
#' TYPE: TEXT FLOAT8 INTEGER BOOL DATE
#' }
#' @param conn A connection object.
#' @param name Table name string, length 1-2.
#' @param temporary Logical.  This is a temporary table or not.
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @returns data.frame. Variables are "name" and "type" (optionally some others)
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbColumnInfo
#' @export
dbServerFieldsCClassesEM <- function(conn, name, temporary = FALSE, display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  table <- dbObjectNameFixEM(conn, o.nm = name, display = display, exec = exec)
  if(temporary) {
    if(length(table) == 2) {
      tableque <- paste0(last(table))
    } else {
      tableque <- paste0(table)
    }
  } else {
    tableque <- paste(table, collapse = ".")
  }

  tmp.query <- paste0("SELECT * FROM ", tableque , " WHERE 1 = 0;")
  ## Display the query
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }

  if(exec) {
    r <- DBI::dbSendQuery(conn, statement = tmp.query)
    Server.fields.C.classes <- DBI::dbColumnInfo(r)
    DBI::dbClearResult(r)
    colnames(Server.fields.C.classes) <- toupper(colnames(Server.fields.C.classes))
    return(Server.fields.C.classes)
  }

  return(invisible(data.frame(DBSERVERFIELDSCCLASSESEM = logical())))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Given an R data.frame, Get The Equivalent Database Server Column Types
#
#' If a data.frame would be promoted to be on a database server, then these would be the equivalent Server datatypes.
#'
#' @details
#' \preformatted{
#'
#' typical SQLite results of TYPE values:
#' "TEXT"  "REAL" "INTEGER"
#'
#' typical PostgreSQL results of TYPE values:
#' "text"  "float8" "bool"  "integer" "date"
#' }
#' @param conn A connection object.
#' @param Df data.frame
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @returns data.frame. Variables are "name" and "type"
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbDataType
#' @export
dfServerFieldsClassesEM <- function(conn, Df, exec = TRUE, display = TRUE) {
tryCatchLog::tryCatchLog({

  if(exec) {
    # as would have been stored on the server
    Server.fields.classes <- data.frame(sapply(Df, function(x) DBI::dbDataType(conn, x)))
    Server.fields.classes <- cAppend(Server.fields.classes, list(name = row.names(Server.fields.classes)), after = 0L)
    colnames(Server.fields.classes) <- c("NAME", "TYPE")
    return(Server.fields.classes)
  }

  return(invisible(data.frame(DFSERVERFIELDSCLASSESEM = logical())))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Given a Server Table, Of Its Fields, Get the Equivalent R Object classes
#
#' If a server table, of its fields,  would be demoted to be in R, then these would be the equivalent R Object classes.
#'
#' R Classes depend upon what the server supports.
#' SQLite R equivalents typically are "character", "integer", and "real".
#' PostgreSQL R equivalents typically are "character", integer", real", "Date", (and may be others: POSIXct?).
#'
#' @param conn A connection object.
#' @param name Table name string, length 1-2. Requires at least one row.
#' @param temporary Logical.  This is a temporary table or not.
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @returns data.frame. Variables are "name" and "type"
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbSendQuery dbClearResult
#' @export
dbRClmnsClassesEM <- function(conn, name, temporary = FALSE, display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  table <- dbObjectNameFixEM(conn, o.nm = name, display = display, exec = exec)
  if(temporary) {
    if(length(table) == 2) {
      tableque <- paste0(last(table))
    } else {
      tableque <- paste0(table)
    }
  } else {
    tableque <- paste(table, collapse = ".")
  }

  tmp.query <- paste0("SELECT * FROM ", tableque , " LIMIT 1;")
  ## Display the query
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }

  if(exec) {
  r <- DBI::dbSendQuery(conn, statement = tmp.query)
    R.zero.rows <- DBI::dbFetch(r, n = 0)
    DBI::dbClearResult(r)
    R.clmns.classes <- data.frame(sapply(R.zero.rows, class))
    R.clmns.classes <- cAppend(R.clmns.classes, list(name = row.names(R.clmns.classes)), after = 0L)
    colnames(R.clmns.classes) <- c("NAME", "TYPE")
    return(R.clmns.classes)
  }

  return(invisible(data.frame(DBRCLMNSCLASSESEM = logical())))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}




#' Add Key or Check Constraint
#'
#' Add a primary or foreign key or check constraint to a table column.
#'
#' @param conn A connection object.
#' @param name A character string, or a character vector, specifying a PostgreSQL table name.
#' @param colname	A character string specifying the name of the column to which the key will be assign; alternatively, a character vector specifying the name of the columns for keys spanning more than one column.
#' @param if.not.exists Logical. Default is FALSE. If TRUE, use pg_ tables to check that in the namespace(schema) the constraint name does not exist.  Needed for the check to work is that parameter "const.name" must also be provided.
#' @param only Logical. Default is FALSE. Whether to add to apply this key just to this parent table(TRUE). Otherwise, also apply this constraint to inherited tables(FALSE).
#' @param const.name String. Name of the constraint.
#' @param type The type of the key, either "primary" or "foreign" or "check" constraint
#' @param check.by If type = "check", then the value of the "check".  Ignored otherwise.
#' @param reference	A character string specifying a foreign table name to which the foreign key will be associated (ignored if type == "primary").
#' @param colref A character string specifying the name of the primary key in the foreign table to which the foreign key will be associated; alternatively, a character vector specifying the name of the columns for keys spanning more than one column (ignored if type == "primary").
#' @param display	Logical. Whether to display the query (defaults to TRUE).
#' @param exec	Logical. Whether to execute the query (defaults to TRUE).
#' @returns TRUE if the key was successfully added.
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbQuoteIdentifier
#' @export
dbAddKeyEM <- function(conn, name, colname, if.not.exists = FALSE, only = FALSE, const.name = characater(),
                       type = c("primary", "foreign" , "check"), check.by = character(), reference, colref,
                       display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  if(if.not.exists && length(const.name)) {

    SchemaAndName <- dbObjectNameFixEM(conn, o.nm = const.name, as.identifier = TRUE, dbQuote = "Literal", display = display, exec = exec)

    Restriction <-
      paste0("
      table_schema    = ", first(SchemaAndName), "
      AND constraint  = ", last(SchemaAndName)
      )

    # pgc.contype
    # 'c' - check constraint
    # 'p' - primary key constraint
    # 'u' - unique constraint
    # 'f' - foreign key constraint
    #  +  - some rare others
    #
    # List all check constraints in PostgreSQL database
    # 2019
    # https://dataedo.com/kb/query/postgresql/list-check-constraints-in-database

    Statement <- paste0("
    SELECT q.* FROM (
      SELECT
           ccu.table_schema as table_schema,
           pgc.conname as constraint,
           pgc.contype,
           ccu.table_name,
           ccu.column_name
      FROM pg_constraint pgc
      JOIN pg_namespace nsp on nsp.oid = pgc.connamespace
      JOIN pg_class  cls on pgc.conrelid = cls.oid
      LEFT JOIN information_schema.constraint_column_usage ccu
                on pgc.conname = ccu.constraint_name
                and nsp.nspname = ccu.constraint_schema
      ORDER BY pgc.conname
    ) q
    WHERE", Restriction, ";")

    Results <- dbGetQueryEM(conn, Statement = Statement, display = display, exec = exec)

    if(!NROW(Results)) {
      message(paste0("Constraint ", toupper(paste0(SchemaAndName, collapse = ".")) , " already exists, so skipping . . ."))
      return(TRUE)
    }

  }

  name <- dbObjectNameFixEM(conn, o.nm = name, display = display, exec = exec)
  nameque <- paste(name, collapse = ".")

  colname <- paste(DBI::dbQuoteIdentifier(conn, colname), collapse = ", ")
  type <- toupper(match.arg(type))
  if (type == "PRIMARY") {
    colref <- ""
    references <- ""
  }
  else if (type == "FOREIGN") {
    colref <- paste(DBI::dbQuoteIdentifier(conn, colref),
                    collapse = ", ")
    reference <- dbObjectNameFixEM(conn, o.nm = reference, display = display, exec = exec)
    references <- paste0(" REFERENCES ", paste(reference,
                                               collapse = "."), " (", colref, ")")
  }
  tmp.query <- paste0("ALTER TABLE ", if(only) " ON ONLY ", nameque, " ADD ", if(length(const.name)) paste0(" CONSTRAINT ", const.name, " "), type,
                      if(type != "check") " KEY ",
                      " (", colname, if(type == "check") paste0(" = ", check.by) , ")", references, ";")

  dbExecuteEM(conn, Statement = tmp.query, display = display, exec = exect)
  if (exec) {
    return(TRUE)
  }

  return(invisible(data.frame(DFADDKEYEM = logical())))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Create an index
#'
#' Defines a new index on a PostgreSQL table.
#'
#' @param conn A connection object.
#' @param name A character string specifying a PostgreSQL table name.
#' @param colname A character string, or a character vector specifying the name of the column to which the key will be associated; alternatively, a character vector specifying the name of the columns to build the index.
#' @param if.not.exists Logical. Default is FALSE. If TRUE, add the " IF NOT EXISTS " clause.
#' @param only Logical. Default is FALSE. Whether to add to apply this key just to this parent table(TRUE). Otherwise, also apply this index to inherited indexes(FALSE).
#' @param idxname A character string specifying the name of the index to be created. By default, this uses the name of the table (without the schema) and the name of the columns as follows: <table_name>_<column_names>_idx.
#' @param unique Logical. Causes the system to check for duplicate values in the table when the index is created (if data already exist) and each time data is added. Attempts to insert or update data which would result in duplicate entries will generate an error.
#' @param method The name of the method to be used for the index. Choices are "btree", "hash", "rtree", and "gist". The default method is "btree", although "gist" should be the index of choice for PostGIS spatial types (geometry, geography, raster).
#' @param display	Logical. Whether to display the query (defaults to TRUE).
#' @param exec	Logical. Whether to execute the query (defaults to TRUE).
#' @returns TRUE if the index was successfully created.
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbQuoteIdentifier
#' @export
dbIndexEM <- function(conn, name, colname, if.not.exists = FALSE, only = FALSE, idxname, unique = FALSE,
                      method = c("btree","hash", "rtree", "gist"),
                      display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  if (missing(idxname)) {
    idxname <- DBI::dbQuoteIdentifier(conn, paste(name[length(name)],
                                                  paste(colname, collapse = "_"), "idx", sep = "_"))
  }
  else {
    idxname <- DBI::dbQuoteIdentifier(conn, idxname)
  }
  name <- dbObjectNameFixEM(conn, o.nm = name, display = display, exec = exec)
  nameque <- paste(name, collapse = ".")
  colname <- paste(DBI::dbQuoteIdentifier(conn, colname), collapse = ", ")
  unique <- ifelse(unique, "UNIQUE ", "")
  method <- match.arg(method)
  usemeth <- ifelse(method == "btree", "", paste(" USING",
                                                 toupper(method)))
  tmp.query <- paste0("CREATE ", unique, "INDEX ", if(if.not.exists) " IF NOT EXISTS ", if(only) " ON ONLY ", idxname,
                      " ON ", nameque, usemeth, " (", colname, ");")

  dbExecuteEM(conn, Statement = tmp.query, display = display, exec = exec)
  if (exec) {
    return(TRUE)
  }

  return(invisible(data.frame(DBINDEXEM = logical())))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}




#' Add or Remove a Column
#'
#' Add or remove a column to/from a table.
#'
#' @param conn A connection object.
#' @param name A character string specifying a PostgreSQL table name.
#' @param colname A character string specifying the name of the column
#' @param action A character string specifying if the column is to be added ("add", default) or removed ("drop").
#' @param coltype A character string indicating the type of the column, if action = "add".
#' @param cascade Logical. Whether to drop foreign key constraints of other tables, if action = "drop".
#' @param display Logical. Whether to display the query (defaults to TRUE).
#' @param exec Logical. Whether to execute the query (defaults to TRUE).
#' @returns TRUE if the column was successfully added or removed.
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbQuoteIdentifier
#' @export
dbColumnEM <- function (conn, name, colname, action = c("add", "drop"), coltype = "integer",
                        cascade = FALSE, display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  name <- dbObjectNameFixEM(conn, o.nm = name, display = display, exec = exec)
  nameque <- paste(name, collapse = ".")
  colname <- DBI::dbQuoteIdentifier(conn, colname)
  action <- toupper(match.arg(action))
  args <- ifelse(action == "ADD", coltype, ifelse(cascade,
                                                  "CASCADE", ""))
  tmp.query <- paste0("ALTER TABLE ", nameque, " ", toupper(action),
                      " COLUMN ", colname, " ", args, ";")

  dbExecuteEM(conn, Statement = tmp.query, display = display, exec = exec)

  if (exec) {
    return(TRUE)
  }

  return(invisible(data.frame(DBCOLUMNEM = logical())))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Add or Remove a Partition
#'
#' From a partitioned table or partitioned index add or remove a partition.
#'
#' @param conn A connection object.
#' @param dbobject A character string specifying if the the type of the parent database object.  This can be "table"(default) or "index".
#' @param name A character string specifying a PostgreSQL already partitioned table or index name. Note, a table can not be converted to a partitioned table and vice-versa.  ALTER has no option.
#' @param partition A character string specifying the name of the partition.
#' @param if.not.exists Logical. Default is FALSE. If TRUE, use pg_ tables to check that in the (potential target partition) object exists and is not attached, before the partitioned table or partitioned index tries to attach to it.
#' @param part.bound.value Of the attaching the partition, the partitions partition bound single value.
#' @param action A character string specifying if the column is to be added ("attach", default) or removed ("drop").
#' @param display Logical. Whether to display the query (defaults to TRUE).
#' @param exec Logical. Whether to execute the query (defaults to TRUE).
#' @returns TRUE if the partition was successfully added or removed.
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbQuoteIdentifier
#' @export
dbAttachPartEM <- function (conn, dbobject = "table", name, partition, if.not.exists = FALSE, part.bound.value, action = c("attach", "drop"),
                            display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  name <- dbObjectNameFixEM(conn, o.nm = name, display = display, exec = exec)
  nameque <- paste(name, collapse = ".")

  partition <- dbObjectNameFixEM(conn, o.nm = partition, display = display, exec = exec)
  partitionque <- paste(partition, collapse = ".")

  if(if.not.exists) {
     Results <- dbListInheritEM(conn, name = name, display = display, exec = exec)
     if(!NROW(Results)) {
       message(past0("Partitioned ", toupper(dbobject), " ", paste0(nameque, collapse = "."), " does not exist, so skipping . . ."))
       return(invisible(TRUE))
     }
  }

  if(if.not.exists) {
    Results <- dbListInheritEM(conn, name = partition, display = display, exec = exec)
    if(!NROW(Results)) {
      message(past0("Target ", toupper(dbobject), " ", paste0(partitionque, collapse = "."), " does not exist, so skipping . . ."))
      return(invisible(TRUE))
    }
  }

  if(if.not.exists) {
    Results <- dbListInheritEM(conn, name = name, display = display, exec = exec)
    if(1) {
      MatchSchemaIdx <- match(noquote(paste0(first(nameque))) %in% Results$CHILD_SCHEMA)
      MatchNameIdx   <- match(noquote(paste0(last(nameque)))  %in% Results$CHILD)
      if(length(match(MatchSchemaIdx %in% MatchNameIdx))) {
         message("Partition object with Partition, relationship already exists, so skipping . . .")
        return(invisible(TRUE))
      }
    }
  }

  action <- toupper(match.arg(action))
  args <- ifelse(action == "ATTACH" && dbobject == "table", paste0(" FOR VALUES IN (" , part.bound.value , ") "), "")

  tmp.query <- paste0("ALTER ", toupper(dbobject) , " ", nameque, " ", action,
                      " PARTITION ", partitionque, " ", args, ";")

  dbExecuteEM(conn, Statement = tmp.query, display = display, exec = exec)

  if (exec) {
    return(TRUE)
  }

  return(invisible(data.frame(DBATTACHPARTEM = logical())))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}





#' Insert or Append to a New or Current Table or Table partition.
#'
#' @description
#' \preformatted{
#' Thick wrapper over R CRAN package caroline function dbWriteTable2.
#'
#' This is R cran package "caroline" function "dbWriteTable2" with modifications
#' JAN 2021
#' https://github.com/cran/caroline/blob
#' /af201137e4a31d675849291a1c9c07a0933b85de/R/database.R
#'
#' modifications
#'
#' 1. add.id domain specific feature is removed
#' 2. pg.update.seq domain specific feature is removed
#' 3. "ORDER BY id DESC LIMIT 1" is replaced by " WHERE FALSE "
#' 4. function "names" upon "df" is replaced with the modern "colnames"
#'    where appropriate
#' 5. function "names" is replaced with the safer "Names"
#' 6. "nv" is replaced with "nameVect".
#' 7. Calls to DBI are called through "PACKAGE::".
#'    These are the (working) dbListFields, dbSendQuery, dbColumnInfo,
#'    dbClearResult, and dbWriteTable
#' 8. If a column exists in the local table.name(data.frame) but does not
#'    exist on the server
#'    then it is added to the server's table fields.
#' 9. Fix and Put back the type mismatch test.
#' 10. Added drop = F to prevent dropping to a vector.
#' 11. Changed message - not in fields of '", table.name,"' table. Omiting
#'                  To - not in fields of '", table.name,"' Adding.
#'
#' For consistency and understandability renamed db.col.info to fields.info.
#' For consistency and understandability renamed db.precisions to fields.precisions.
#' For consistency and understandability renamed db.sclasses to fields.sclasses.
#' Since static, moved closer to the top calls to DBI::dbSendQuery, DBI::dbColumnInfo and DBI::dbClearResult
#' Moved other non-changing code to near the top.
#' }
#' @param conn A DBIConnection object, as returned by dbConnect().
#' @param DfName  String.  Default is substitute(Df). The name of the table to which the data frame is to be loaded.
#' @param Df, data.frame. Required. To be loaded to the database.
#' @param PartitionOf String.  If the table is (or to be) a participant as a Partition, then this table is a partion of the this partitioned table "PartitionOf".
#' @param PartBoundValue String. If the table is (or to be) a participant as a Partition, then this is a partition bound as a List value.
#' @param PartKeyCol String.  If the table is (or to be is) a Partitioned table is of that List partition and the partition key column.
#' @param PrimaryKey Vector of Strings.  If present, then of the [to be] primary-keyed table, this is the the vector values (in order) are the primary-keyed columns. The name of the primary key is taken from the name of the vector.
#' @param Indexes List of "Vector of Strings".  If present, then of the [to be] indexed table, this is the the vector values (in order) are the indexed columns. The name of the index is taken from the name of the vector.
#' @param lowerDfName Logical. Default is TRUE. Make the target database table name to be in lowercase.
#' @param lowerColNames Logical. Default is TRUE. Make the target database table column names to be in lowercase.
#' @param replaceDotUsingUnderscore Logical. Default is TRUE. Make the target database table column names internal "dots" be converted to underscores(_).
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).'
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
#' dbWriteTableEM(get("connEM"), DfName = "mtcars",  Df = mtcars2s, PartKeyDef = "LIST (gear)", PrimaryKey = c("gear", "model"), Indexes = list(gear_model_vs = c("gear", "model", "vs")))
#'
#' # Appends data (with the Df having less columns that the server database).
#' # Those server columns, that are not found in the Df, are added to the Df.
#' mtcars2lDf <- mtcars2[6:10, "model", drop = F]
#' dbWriteTableEM(get("connEM"), DfName = "mtcars", Df = mtcars2lDf)
#'
#' # Appends data (with the server database having less columns that the Df).
#' # Those Df columns, that are not found in the server, are added to the sever.
#' mtcars2lSv <- {DfNew <- mtcars2[11:15, c("model","vs", "am", "gear", "carb")]
#'                colnames(DfNew) <- paste0(colnames(DfNew),"_new")
#'                DfNew[["model"]] <- DfNew[["model_new"]]
#'                DfNew <- DataCombine::MoveFront(DfNew, Var = "model")
#'                DfNew
#'               }; rm(DfNew)
#' dbWriteTableEM(get("connEM"), DfName = "mtcars", Df = mtcars2lSv)
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbExistsTable dbWriteTable
#' @export
dbWriteTableEM <- function(conn, DfName = substitute(Df), Df,
                           PartitionOf = character(), PartBoundValue = character(), PartKeyCol = character(),
                           PrimaryKey = character(), Indexes = list(),
                           lowerDfName = TRUE, lowerColNames = TRUE, replaceDotUsingUnderscore = TRUE,
                           display = TRUE, exec = TRUE,
                           ...) {
tryCatchLog::tryCatchLog({

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

  if(!DBI::dbExistsTable(conn, name = DfName)) {
    # table does not exist
    # create partitioned table ("p" - partitioned table) (c1)
    #
    # just need the structure (not the data)
    if(length(PartitionOf)) {
      SplittedDf <- split(Df, f = Df[[PartKeyCol]])

      # create the parent partitioned table and its indexes onlies
      if(length(SplittedDf)) {
        # empty partitioned parent table
        dbCreatePartBoundTableEM(conn, name = DfName, fields = Df,
                                 part.key.def = if(length(PartKeyCol)) { paste0(" LIST (", PartKeyCol, ") ") } else { character() },
                                 display = display, exec = exec)

        # create the partitioned parent table index onlies
        mapply(function(Index, IndexName) {
          dbIndexEM(conn, name = DfName, colname = Index, only = TRUE, idxname = paste0(DfName , "_", IndexName, "_idx"))
        }, Indexes, Names(Indexes), SIMPLIFY = FALSE)

        if(length(PrimaryKey)) {
          # alter table add constraint x primary key(default)
          # so, I can do global . . .
          # INSERT INTO a . . . ON CONFLICT ON CONSTRAINT a_pk DO UPDATE SET . . .
          dbAddKeyEM(conn, name = DfName, colname = PrimaryKey, const.name = paste0(DfName, "_pk"),
                     display = display, exec = exec)
        }

      }

      # per each chunk part
      mapply(function(DfPart, DfPartBoundValue) {

        DfPartNameSuffix <- try(as.integer(DfPartBoundValue), silent = TRUE)
        if(inherits(DfPartNameSuffix, "try-error")) {
          DfPartNameSuffix <- try(as.character(DfPartBoundValue), silent = TRUE)
          if(inherits(DfPartNameSuffix, "try-error")) {
            stop("Can not create an integer nor a character partition \"DfPartNameSuffix\".")
          }
        }
        DfPartName <- paste0(DfName, "_", DfPartNameSuffix)

        # create an empty table (to be a future partition)
        dbCreatePartBoundTableEM(conn, name = DfPartName, like = DfName,  like.name.defaults = TRUE,
                                 display = display, exec = exec)

        # create empty table indexes
        mapply(function(Index, IndexName) {
          dbIndexEM(conn, name = DfPartName, colname = Index, only = TRUE, idxname = paste0(DfPartName , "_", IndexName, "_idx"))
        }, Indexes, Names(Indexes), SIMPLIFY = FALSE)

        if(length(PrimaryKey)) {
          # alter table add constraint x primary key(default)
          dbAddKeyEM(conn, name = DfPartName, colname = PrimaryKey, const.name = paste0(DfPartName, "_pk"),
                     display = display, exec = exec)
        }

        # alter table add constraint x check
        dbAddKeyEM(conn, name = DfPartName, colname = PartKeyCol, const.name = paste0(DfPartName, "_chk"),
                   type = "check", check.by = DfPartBoundValue,
                   display = display, exec = exec)

        # DfPart: data to be loaded
        DfPart <- dbdfMatchColsEM(conn, name = DfName, value = DfPart, display = display, exec = exec)

        # col.names: a character vector with column names; column names are quoted to work as SQL identifiers. Thus, the column names are case sensitive and make.db.names will NOT be used here.
        # field.types: is a list of named field SQL types where names(field.types) provide the new table's column names (if missing, field types are inferred using dbDataType).
        # RPostgreSQL/html/dbReadTable-methods.html
        DBI::dbWriteTable(conn, name = DfName, value = DfPart, append = TRUE, row.names = FALSE)

        # alter "partition table" attach partition
        dbAttachPartEM(conn, name = DfName, partition = DfPartName, part.bound.value = DfPartBoundValue,
                       display = display, exec = exec)

        # alter "partition index" attach partition
        mapply(function(IndexName) {

          dbAttachPartEM(conn, dbobject = "index",
                         name =  paste0(DfName , "_", IndexName, "_idx"),
                         partition = paste0(DfPartName , "_", IndexName, "_idx"),
                         display = display, exec = exec)

        }, Names(Indexes), SIMPLIFY = FALSE)

      }, SplittedDf, Names(SplittedDf), SIMPLIFY = FALSE)

    }

  } else { # already exists

    DetectedPartKeyCol  <- dbPartKeyColEM(conn, DfName = DfName)
    SplittedDf <- split(Df, f = Df[[DetectedPartKeyCol]])

    # per each chunk part
    mapply(function(DfPart, DfPartBoundValue) {

      DfPartNameSuffix <- try(as.integer(DfPartBoundValue), silent = TRUE)
      if(inherits(DfPartNameSuffix, "try-error")) {
        DfPartNameSuffix <- try(as.character(DfPartBoundValue), silent = TRUE)
        if(inherits(DfPartNameSuffix, "try-error")) {
          stop("Can not create an integer nor a character partition \"DfPartNameSuffix\".")
        }
      }
      DfPartName <- paste0(DfName, "_", DfPartNameSuffix)

      # create an empty table (to be a future partition)
      dbCreatePartBoundTableEM(conn, if.not.exists = TRUE, name = DfPartName, like = DfName,  like.name.defaults = TRUE,
                               display = display, exec = exec)

      # create empty table indexes
      mapply(function(Index, IndexName) {
        dbIndexEM(conn, name = DfPartName, colname = Index, if.not.exists = TRUE, only = TRUE, idxname = paste0(DfPartName , "_", IndexName, "_idx"))
      }, Indexes, Names(Indexes), SIMPLIFY = FALSE)

      if(length(PrimaryKey)) {
      # alter table add constraint x primary key(default)
      dbAddKeyEM(conn, name = DfPartName, colname = PrimaryKey, if.not.exists = TRUE,
                 const.name = paste0(DfPartName, "_pk"),
                 display = display, exec = exec)
      }

      # alter table add constraint x check
      dbAddKeyEM(conn, name = DfPartName, colname = PartKeyCol, if.not.exists = TRUE,
                 const.name = paste0(DfPartName, "_chk"),
                 type = "check", check.by = DfPartBoundValue,
                 display = display, exec = exec)


      # DfPart: data to be loaded
      DfPart <- dbdfMatchColsEM(conn, name = DfName, value = DfPart, display = display, exec = exec)

      # col.names: a character vector with column names; column names are quoted to work as SQL identifiers. Thus, the column names are case sensitive and make.db.names will NOT be used here.
      # field.types: is a list of named field SQL types where names(field.types) provide the new table's column names (if missing, field types are inferred using dbDataType).
      # RPostgreSQL/html/dbReadTable-methods.html
      DBI::dbWriteTable(conn, name = DfName, value = DfPart, append = TRUE, row.names = FALSE)

      # alter "partition table" attach partition
      dbAttachPartEM(conn, name = DfName, partition = DfPartName, if.not.exists = TRUE, part.bound.value = DfPartBoundValue,
                     display = display, exec = exec)

      # alter "partition index" attach partition
      mapply(function(IndexName) {

        dbAttachPartEM(conn, dbobject = "index",
                       name =  paste0(DfName , "_", IndexName, "_idx"),
                       partition = paste0(DfPartName , "_", IndexName, "_idx"),
                       if.not.exists = TRUE,
                       display = display, exec = exec)

      }, Names(Indexes), SIMPLIFY = FALSE)

    }, SplittedDf, Names(SplittedDf), SIMPLIFY = FALSE)

  }

  return(invisible(data.frame(DBWRITETABLEEM = logical())))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}







#' Create New Columns In the Data.frame or On the Server Table
#'
#' Match the the Server/data.frame columns with each other.
#' Create new columns as necessary. Verify/Make the data.frame
#' columns order match that of the Server table order.
#' Inspired by the R CRAN package Caroline.
#'
#' @param conn A DBIConnection PostgreSQL object, as returned by DBI::dbConnect().
#' @param name a character string specifying a table name.
#' @param value a data.frame (or coercible to data.frame).
#' @param temporary Logical. Default is FALSE, If TRUE, then look for the "conn" object "name" in the temporary namespace.
#' @param display	Logical. Whether to display the query (defaults to TRUE).
#' @param exec	Logical. Whether to execute the query (defaults to TRUE).
#' @return data.frame with more columns and/or re-ordered columns. The server "name" may get more columns.
#' @examples
#' \dontrun{
#'
#' conn <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), user = "postgres")
#'
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
#' dbdfMatchColsEM(conn, DfName = "mtcars",  Df = mtcars2s, PartKeyDef = "LIST (gear)", PrimaryKey = c("gear", "model"), Indexes = list(gear_model_vs = c("gear", "model", "vs")))
#'
#' # Appends data (with the Df having less columns that the server database).
#' # Those server columns, that are not found in the Df, are added to the Df.
#' mtcars2lDf <- mtcars2[6:10, "model", drop = F]
#' dbdfMatchColsEM(conn, DfName = "mtcars", Df = mtcars2lDf)
#'
#' # Appends data (with the server database having less columns that the Df).
#' # Those Df columns, that are not found in the server, are added to the sever.
#' mtcars2lSv <- {DfNew <- mtcars2[11:15, c("model","vs", "am", "gear", "carb")]
#'                colnames(DfNew) <- paste0(colnames(DfNew),"_new")
#'                DfNew[["model"]] <- DfNew[["model_new"]]
#'                DfNew <- DataCombine::MoveFront(DfNew, Var = "model")
#'                DfNew
#'               }; rm(DfNew)
#'
#' dbdfMatchColsEM(conn, DfName = "mtcars", Df = mtcars2lSv)
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo as.Date
#' @export
dbdfMatchColsEM <- function(conn, name = substitute(value), value, temporary = FALSE,
                            display = TRUE, exec = TRUE) {
  tryCatchLog::tryCatchLog({

  Df <- value

  ## look for unloadable columns in the df
  colnames(Df) <- tolower(colnames(Df))
  colnames(Df) <- gsub("[.]",'_',colnames(Df))

  # add columns to the data.frame (Df),
  # that exist on the server table (name),
  # but do not exist in the data.frame (Df)

  ColsToAddtoDfIndex <-
    !dbRClmnsClassesEM(conn, name = name, temporary = temporary, display = display, exec = exec)$NAME  %in%
     dfServerFieldsClassesEM(conn, Df = Df, display = display, exec = exec)$NAME

  as.Date <- zoo::as.Date
  mapply(function(Name, Type) {
    Df[[Name]] <- rep(NA, NROW(Df))
    DF[[Name]] <- eval(parse(text = paste0("as.", Type, "(DF[[Name]])")))
    invisible()
  },
  dbRClmnsClassesEM(conn, name = name, temporary = temporary, display = display, exec = exec)$NAME[ColsToAddtoDfIndex],
  dbRClmnsClassesEM(conn, name = name, temporary = temporary, display = display, exec = exec)$TYPE[ColsToAddtoDfIndex],
  SIMPLIFY = FALSE)

  # add columns to the server table (name),
  # that exist on the data.frame (Df),
  # but do not exist in the server table (name)

  ColsToAddtoServerIndex <-
    !dfServerFieldsClassesEM(conn, Df = Df, display = display, exec = exec)$NAME %in%
     dbRClmnsClassesEM(conn, name = name, display = display, exec = exec)$NAME

  mapply(function(Name, Type) {
    dbColumnEM(conn, name = name, colname = Name, coltype = Type, display = display, exec = exec)
    invisible()
  },
  dfServerFieldsClassesEM(conn, Df = Df, display = display, exec = exec)$NAME[ColsToAddtoServerIndex],
  dfServerFieldsClassesEM(conn, Df = Df, display = display, exec = exec)$TYPE[ColsToAddtoServerIndex],
  SIMPLIFY = FALSE)

  # in the data.frame, re-order the  columns
  # to match those of the server order
  Df <- Df[, cSort(colnames(Df), InitOrder = dbRClmnsClassesEM(conn, name = name, display = display, exec = exec)$NAME)
           , drop = F]

  return(Df)

  return(invisible(data.frame(DBDFMATCHOLSEM = logical())))

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}
