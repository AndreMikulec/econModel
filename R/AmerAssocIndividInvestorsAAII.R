
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











#' PostgreSQL Performance
#'
#' Set PostgreSQL memory parameters.
#'
#' @param conn PostgreSQL DBI connection.
#' @returns PostgreSQL parameters are set
#' @examples
#' \dontrun{
#' dbSetPerformanceEM(get("connEM"))
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbSetPerformanceEM <- function(conn) {
tryCatchLog::tryCatchLog({

  # DELL HOME "COMPUTER" ( 2017 / WINDOWS 10 PROFESSIONAL )
  # HAS 16GB of RAM AND 4 CORES

  # EXPERIMENT ( memory for disk caching ) -- 2048(GUESSING) + 4096(shared buffers)
  dbExecuteEM(conn, Statement = "SET effective_cache_size  TO '6144MB';")

  # windows LIMIT 2047
  # A good rule of thumb is to keep: work_mem*max_connections*2 < 1/4 of memory
  # NOTE: WATCH OUT FOR 'R language: parallel. Each process GETS 'work_mem' limit
  dbExecuteEM(conn, Statement = "SET work_mem TO '2047MB';")

  # maximum amount of memory to be used by maintenance operations, such as VACUUM, CREATE INDEX, and ALTER TABLE ADD FOREIGN KEY
  # https://wiki.postgresql.org/wiki/Tuning_Your_PostgreSQL_Server
  dbExecuteEM(conn, Statement = "SET maintenance_work_mem TO '2047MB';")

  # Controls the query planner's use of table constraints
  # to optimize queries.
  dbExecuteEM(conn, Statement = "SET constraint_exclusion TO partition;")

  # excludes (prunes) the partition from the query plan
  # can also be applied [not only to query planning and] during query execution
  dbExecuteEM(conn, Statement = "SET enable_partition_pruning TO on;")

  # Postgresql 9.6
  dbExecuteEM(conn, Statement = "SET max_parallel_workers_per_gather TO 4;")

  return(TRUE)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Test The RPostgreSQL Connection
#'
#' Is the connection not expired or not valid?
#'
#' @param conn PostgreSQL DBI connection.
#' @param ... Dots passed.
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @returns TRUE(connected) or FALSE(otherwise)
#' @examples
#' \dontrun{
#' isConnectedEM(conn)
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbIsConnectedEM <- function(conn, display = TRUE, exec = TRUE, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  if(missing(conn)) {
    stop(paste0("Parameter \"conn\" is required."))
  }

  Results <- list()

  if(inherits(conn, "PostgreSQLConnection")) {

    One <-try({dbGetQueryEM(conn, Statement = "SELECT 1;", display = display, exec = exec)}, silent = TRUE)
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
#' Adds extra information: current_schema, search_path, temp_dbname, econmodel_db_dbname, client_encoding, and time_zone.
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

    Results["current_schema"] <- unlist(tolower(dbGetQueryEM(conn, Statement = "SELECT current_schema();")))
    Results["search_path"]    <- unlist(tolower(dbGetQueryEM(conn, Statement = "SHOW SEARCH_PATH;")))
    InterimResult             <- unlist(tolower(dbGetQueryEM(conn, Statement = "SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema();")))
    if(length(InterimResult)) {
      Results["temp_dbname"]  <- InterimResult
    }
    if(length(getOption("econmodel_db_dbname"))) {
       Results[["econmodel_db_dbname"]] <- getOption("econmodel_db_dbname")
    }

    Results["client_encoding"] <- unlist(tolower(dbGetQueryEM(conn, Statement = "SHOW client_encoding;")))
    Results["time_zone"] <- unlist(tolower(dbGetQueryEM(conn, Statement = "SHOW TIMEZONE;")))
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
#' @param auto.assign Logical. Should the results be loaded to "env" as variable "connEM" If FALSE, then results are returned from the function  normally.
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @param ... Dots passed.
#' @returns DBI connection object named "connEM" is created, connected and assigned to the environment "env".
#' @examples
#' \dontrun{
#' dbConnectEM()
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbConnect
#' @export
dbLoginEM <- function(driver, user, password = user, host, dbname = user, port,
                        tty, options, forceISOdate, connName, env, auto.assign = TRUE, display = TRUE, exec = TRUE, ...) {
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
      connNameConnected <- dbIsConnectedEM(get(connName, envir = env, inherits = FALSE))
    }

    haveConnLocal <- FALSE
    if(connNameExists && connNameConnected) {
      # creates a reference
      conn <- try({get(connName, envir = env, inherits = FALSE)}, silent = TRUE)
    }
    if(!inherits(conn, "try-error")) {
      haveConnLocal <- TRUE

      tmp.query <- "SET client_encoding TO 'UTF8';"
      ## Display the query
      if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(tmp.query)
      }

      ## Execute the query and return TRUE
      if (exec) {
        try({DBI::dbExecute(conn, statement = tmp.query, ...)})
      }

    }

    if( !haveConnLocal
        ||
        (
        !connNameExists
         ||
        (connNameExists && !connNameConnected)
        )
    ) {
      conn <- try({DBI::dbConnect(drv,
                                         user = user, password = password,
                                         host = host, dbname = dbname, port = port,
                                         tty = tty, options = dboptions, forceISOdate = forceISOdate)},
                  silent = TRUE)
    }
    if(!inherits(conn, "try-error") && dbGetInfo(conn)$user == user) {
      message(paste0("Successfully connected to user \"", user, "\"."))

      tmp.query <- "SET client_encoding TO 'UTF8';"
      ## Display the query
      if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(tmp.query)
      }

      ## Execute the query and return TRUE
      if (exec) {
        try({DBI::dbExecute(conn, statement = tmp.query, ...)})
      }

      if(!dbExistsSchemaEM(conn, schema = user)) dbCreateSchemaEM(conn, schema = user)
      # creates a reference
      if(auto.assign) {
        assign(connName, conn, envir = env)
        message(paste0("Connection R object \"connEM\" has been returned to ", ReturnTo, "."))
      } else {
        return(conn)
      }
      # "r_user_econmodel" in the "r_user_econmodel" database
      # tries to create the schema "r_user_econmodel"
      # do not Disconnect
    }
    else {
      # try another connection
      if(haveConnLocal) {
        DBI::dbDisconnect(conn)
      }
      conn <- try({DBI::dbConnect(drv,
                                 user = "r_user_econmodel", password = "r_user_econmodel",
                                 host = host, dbname = "r_user_econmodel", port = port,
                                 tty = tty, options = dboptions, forceISOdate = forceISOdate)},
                  silent = TRUE)

      if(!inherits(conn, "try-error")) {

        message(paste0("Successfully connected to user \"", "r_user_econmodel", "."))

        tmp.query <- "SET client_encoding TO 'UTF8';"
        ## Display the query
        if (display) {
          message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
          message(tmp.query)
        }

        ## Execute the query and return TRUE
        if (exec) {
          try({DBI::dbExecute(conn, statement = tmp.query, ...)})
        }

        if(!dbExistsSchemaEM(conn, schema = "r_user_econmodel")) dbCreateSchemaEM(conn, schema = "r_user_econmodel")
        # user = user

        if(!dbExistsUserEM( conn, user   = user))    dbCreateUserEM(conn,   user   = user)
        if(!dbExistsDbaseEM(conn, dbname = dbname))  dbCreateDbaseEM(conn,  dbname = user)
        DBI::dbDisconnect(conn)
        #
        # as "ruser"
        # login to the database "user" and and create the "user" schema
        #
        conn <- try({DBI::dbConnect(drv,
                                    user = user, password = password,
                                    host = host, dbname = user, port = port,
                                    tty = tty, options = dboptions, forceISOdate = forceISOdate)},
                    silent = TRUE)
        if(!inherits(conn, "try-error")) {
          # "user" in the "user" database

          tmp.query <- "SET client_encoding TO 'UTF8';"
          ## Display the query
          if (display) {
            message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
            message(tmp.query)
          }

          ## Execute the query and return TRUE
          if (exec) {
            try({DBI::dbExecute(conn, statement = tmp.query, ...)})
          }

          tmp.query <- "SET timezone TO 'UTC';"
          ## Display the query
          if (display) {
            message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
            message(tmp.query)
          }

          ## Execute the query and return TRUE
          if (exec) {
            try({DBI::dbExecute(conn, statement = tmp.query, ...)})
          }

          # tries to create the schema "user"
          if(!dbExistsSchemaEM(conn, schema = user)) dbCreateSchemaEM(conn, schema = user)
          DBI::dbDisconnect(conn)
          # login as "user" to its own database "user"
          dbConnectEM(connName = connName, env = env)
        } else {
          stop(paste0("\"r_user_econmodel\" could not connect to the user database ", user))
        }

      } else {
        # try another connection
        conn <- try({DBI::dbConnect(drv,
                                   user = "postgres", password = "postgres",
                                   host = host, dbname = "postgres", port = port,
                                   tty = tty, options = dboptions, forceISOdate = forceISOdate)},
                    silent = TRUE)
        if(!inherits(conn, "try-error")) {
          message(paste0("Successfully connected to user \"", "postgres", "\"."))

          tmp.query <- "SET client_encoding TO 'UTF8';"
          ## Display the query
          if (display) {
            message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
            message(tmp.query)
          }

          ## Execute the query and return TRUE
          if (exec) {
            try({DBI::dbExecute(conn, statement = tmp.query, ...)})
          }

          tmp.query <- "SET timezone TO 'UTC';"
          ## Display the query
          if (display) {
            message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
            message(tmp.query)
          }

          ## Execute the query and return TRUE
          if (exec) {
            try({DBI::dbExecute(conn, statement = tmp.query, ...)})
          }


          if(!dbExistsUserEM( conn, user   = "r_user_econmodel")) dbCreateUserEM(conn, user = "r_user_econmodel", attributes = c("LOGIN", "CREATEDB", "CREATEROLE"))
          if(!dbExistsDbaseEM(conn, dbname = "r_user_econmodel")) dbCreateDbaseEM(conn, dbname = "r_user_econmodel")
          DBI::dbDisconnect(conn)
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
#' dbExecuteEM(get("connEM"), Statement = "CREATE TABLE xyz();")
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
    Results <- try({DBI::dbExecute(conn, statement = tmp.query)}, silent = T)
    if(inherits(Results, "try-error")) {
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
#' dbDisconnectEM(connName = "connEM", env = .Globalenv)
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
  TRUE
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


#' Disconnect from the econModel database
#'
#  Disconnect and remove the DBI connection object connName (default is "connEM") from the environment "env".
#'
#' @param connName String.  Name of the database connection object. Optional. This may be "passed" instead of  "conn".
#' @param env Environment. Default is the global environment .GlobalEnv.  Location of the connection object "connName".
#' @param ... Dots passed.
#' @returns Disconnects "connEM" from the database or disconnects "connName" from the database and removes it from the environment "env".
#' @examples
#' \dontrun{
#' dbLogoutEM() # default is connection variable "connEM" in the .GlobalEnv
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbLogoutEM <- function(connName, env, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  if(missing(connName)) {
    connName <- "connEM"
  }

  if(missing(env)) {
    env <- .GlobalEnv
  }

  dbDisconnectEM(connName = connName, env = env)

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
  tmp.query <- paste0("SELECT EXISTS(SELECT usename FROM pg_catalog.pg_user WHERE usename = ", DBI::dbQuoteLiteral(conn, x = user), ");")
  ## Display the query
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }

  ## Execute the query and return TRUE
  if (exec) {
    Result <- dbGetQueryEM(conn, Statement = tmp.query)
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
    password <- paste0("password ", DBI::dbQuoteLiteral(conn, x = password))
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
    if(!dbExistsUserEM(conn, user = user)) {
      Results <- try({dbExecuteEM(conn, Statement = tmp.query)})
      if(!inherits(Results, "try-error")) {
        return(TRUE)
      } else {
        stop("Failed to create the user.")
      }
    } else {
      stop(paste0("User ", DBI::dbQuoteLiteral(conn, x = user), " is already in the database."))
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
  tmp.query <-  paste0("SELECT EXISTS(SELECT 1 FROM pg_catalog.pg_namespace WHERE nspname = ", DBI::dbQuoteLiteral(conn, x = schema), ");")
  ## Display the query
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }

  ## Execute the query and return TRUE
  if (exec) {
    Result <- dbGetQueryEM(conn, Statement = tmp.query)
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
    if(!dbExistsSchemaEM(conn, schema = schema)) {
      Results <- try({dbExecuteEM(conn, Statement = tmp.query)})
      if(inherits(Results, "try-error")) {
        stop("Failed to create the schema.")
      }
    } else {
      stop(paste0("Schema ", DBI::dbQuoteLiteral(conn, x = user), " is already in the database."))
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
      Results <- try({dbExecuteEM(conn, Statement = tmp.query)})
      if(inherits(Results, "try-error")) {
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
  tmp.query <- paste0("SELECT EXISTS(SELECT 1 FROM pg_catalog.pg_database WHERE datname = ", DBI::dbQuoteLiteral(conn, x = dbname), ");")
  ## Display the query
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }

  ## Execute the query and return TRUE
  if (exec) {
    Result <- dbGetQueryEM(conn, Statement = tmp.query)
    if(unlist(Result)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

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
#' @keywords internal
#' @importFrom DBI dbQuoteIdentifier dbQuoteLiteral
#' @importFrom DBI dbQuoteString
#' @examples
#' \dontrun{
#' name <- c("schema","table")
#' dbObjectNameFix(conn,name)
#'
#' #current search path schema is added to single-length character object (if only table is given)
#' name<-"table"
#' dbObjectNameFix(conn,name)
#'
#' #schema or table names with double quotes should be given exactly as they are
#' (make sure to wrap in single quotes in R):
#' name <- c('sch"ema','"table"')
#' dbObjectNameFix(conn,name)
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbGetQuery dbQuoteIdentifier ANSI
dbObjectNameFix <- function(conn = NULL, o.nm, as.identifier = TRUE, dbQuote = "Identifier") {
tryCatchLog::tryCatchLog({
  # case of no schema provided
  if (length(o.nm) == 1 && !is.null(conn) && !inherits(conn, what = "AnsiConnection")) {
    schemalist <- DBI::dbGetQuery(conn,"SELECT nspname AS s FROM pg_catalog.pg_namespace;")$s
    user <- DBI::dbGetQuery(conn,"SELECT CURRENT_USER AS user;")$user
    schema <- DBI::dbGetQuery(conn,"SHOW SEARCH_PATH;")$search_path
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
  CurrentUser <- unlist(dbGetQueryEM(conn, Statement = "SELECT CURRENT_USER;"))
  tmp.query <- paste0("GRANT ", owner, " TO ", CurrentUser, ";")
  ## Display the query
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }
  ## Execute the query and return TRUE
  if (exec) {
    Results <- try({dbExecuteEM(conn, Statement = tmp.query) })
    if (inherits(Results, "try-error")) {
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
    if(!dbExistsDbaseEM(conn, dbname = dbname)) {
      Results <- try({dbExecuteEM(conn, Statement = tmp.query)})
      if(!inherits(Results, "try-error")) {
        return(TRUE)
      } else {
        stop("Failed to create the database.")
      }
    } else {
      stop(paste0("Database ", DBI::dbQuoteLiteral(conn, x = dbname), " is already in the cluster."))
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
    if(!dbExistsDbaseEM(conn, dbname = dbname)) {
      Results <- try({dbExecuteEM(conn, Statement = tmp.query)})
      if(!inherits(Results, "try-error")) {
        return(TRUE)
      } else {
        stop("Failed to alter the database.")
      }
    } else {
      stop(paste0("Database ", DBI::dbQuoteLiteral(conn, x = dbname), " can not be alterned from here."))
    }
  }

  invisible()

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
  NotDatabase <- DBI::dbQuoteLiteral(conn, x = getOption("econmodel_db_dbname"))
  tmp.query <-  paste0("SELECT datname FROM pg_catalog.pg_database WHERE datname LIKE 'rtmp%' AND datname != ", NotDatabase, ";")
  ## Display the query
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }
  ## Execute the query
  if(exec) {
    Databases   <- dbGetQueryEM(conn, Statement = tmp.query)
  }
  Databases   <- unlist(Databases)
  dbDisconnectEM(conn)

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
        Results <- dbExecuteEM(conn, Statement = tmp.query)
        if(inherits(Results, "try-error")) {
          message(paste0("Failed to drop database ", dbname, "."))
        }
      }
      dbDisconnectEM(conn)

    }, silent = TRUE)
    invisible()
  })

  conn <- DBI::dbConnect(drv, user = "r_user_econmodel", password = "r_user_econmodel", dbname = "r_user_econmodel")
  lapply(Databases, function(dbname) {
    try({
      # trying to drop those users (with the same name as the database)
      tmp.query <- paste0("DROP USER ", dbname, ";")
      ## Display the query
      if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(tmp.query)
      }
      ## Execute the query
      if(exec) {
        Results <- dbExecuteEM(conn, Statement = tmp.query)
        if(inherits(Results, "try-error")) {
          message(paste0("Failed to drop user ", dbname, "."))
        }
      }

    }, silent = TRUE)
    invisible()
  })
  dbDisconnectEM(conn)

  options(ops)
  return(TRUE)

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
#' @param ... Dots passed.
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
dbListInheritEM <- function(conn, name, side = "parent", display = TRUE, exec = TRUE, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  if(missing(conn)) {
    stop("Parameter \"conn\" is required.")
  }

  if(missing(name)) {
    stop("Parameter \"name\" is required.")
  }

  SchemaAndName <- dbObjectNameFix(conn, o.nm = name, as.identifier = TRUE, dbQuote = "Literal")

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
    return(Results[, , drop = FALSE])
  } else {
    return(data.frame(list(PARENT_SCHEMA = "", PARENT = "", PARENT_RELKIND = "", PARENT_PART_KEY_DEF = "", PARENT_PART_BOUND = "", INHSEQNO = 0L, CHILD_SCHEMA = "", CHILD = "", CHILD_REL_KIND = "", CHILD_PART_KEY_DEF = "", CHILD_PART_BOUND = ""))[FALSE, , drop = F])
  }

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
#' @param name	The column name you would like to pull out as a named vector. OR the names of the vector (if x is a vector)
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

  table <- dbObjectNameFix(conn, o.nm = name)
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
    like.table <- dbObjectNameFix(conn, o.nm = like.name)
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
    dbExecuteEM(conn, Statement = tmp.query, display = display, exec = exec, ...)
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

  query <- paste0(
    "CREATE ", if (temporary) "TEMPORARY ", "TABLE ", if(if.not.exists) " IF NOT EXISTS ",
    tableque, " (\n",
    "  ", paste(fields, collapse = ",\n  "), " \n)", part.by, part.bound, part.key.def, "\n"
  )

  dbExecuteEM(conn, Statement = query, display = display, exec = exec, ...)
  invisible(TRUE)

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
#' @returns vector of size 1 of the partition key definition.
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbPartKeyColEM <- function(conn, DfName) {
tryCatchLog::tryCatchLog({
  DetectedPartKeyDef <- character()
  Results <- dbListInheritEM(conn, DfName)
  if(!NROW(Results)) {
    stop(paste0("Object ", DfName, " is missing from the database."))
  }
  SubResults <- dbListInheritEM(conn, name = DfName)$PARENT_PART_KEY_DEF
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
#' @returns vector of partiton bound list values.
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbPartBoundEM <- function(conn, DfName) {
tryCatchLog::tryCatchLog({
  DetectedPartBound <- character()
  Results <- dbListInheritEM(conn, DfName)
  if(!NROW(Results)) {
    stop(paste0("Object ", DfName, " is missing from the database."))
  }
  SubResults <- dbListInheritEM(conn, name = DfName)$PARENT_PART_BOUND
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
#' @returns data.frame. Variables are "name" and "type" (optionally some others)
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbServerFieldsCClassesEM <- function(conn, name, temporary = FALSE) {
tryCatchLog::tryCatchLog({

  table <- dbObjectNameFix(conn, o.nm = name)
  if(temporary) {
    if(length(table) == 2) {
      tableque <- paste0(last(table))
    } else {
      tableque <- paste0(table)
    }
  } else {
    tableque <- paste(table, collapse = ".")
  }

  r <- DBI::dbSendQuery(conn, paste0("SELECT * FROM ", tableque , " WHERE 1 = 0;"))
  Server.fields.C.classes <- DBI::dbColumnInfo(r)
  DBI::dbClearResult(r)
  colnames(Server.fields.C.classes) <- toupper(colnames(Server.fields.C.classes))
  return(Server.fields.C.classes)

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
#' @returns data.frame. Variables are "name" and "type"
#' @importFrom tryCatchLog tryCatchLog
#' @export
dfServerFieldsClassesEM <- function(conn, Df) {
tryCatchLog::tryCatchLog({

  # as would have been stored on the server
  Server.fields.classes <- data.frame(sapply(mtcars2s, function(x) DBI::dbDataType(conn, x)))

  Server.fields.classes <- cAppend(Server.fields.classes, list(name = row.names(Server.fields.classes)), after = 0L)
  colnames(Server.fields.classes) <- c("NAME", "TYPE")
  return(Server.fields.classes)

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
#' @returns data.frame. Variables are "name" and "type"
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbRClmnsClassesEM <- function(conn, name, temporary = FALSE) {
tryCatchLog::tryCatchLog({

  table <- dbObjectNameFix(conn, o.nm = name)
  if(temporary) {
    if(length(table) == 2) {
      tableque <- paste0(last(table))
    } else {
      tableque <- paste0(table)
    }
  } else {
    tableque <- paste(table, collapse = ".")
  }

  r <- DBI::dbSendQuery(conn, paste0("SELECT * FROM ", tableque , " LIMIT 1;"))
  R.zero.rows <- DBI::dbFetch(r, n = 0)
  DBI::dbClearResult(r)
  R.clmns.classes <- data.frame(sapply(R.zero.rows, class))

  R.clmns.classes <- cAppend(R.clmns.classes, list(name = row.names(R.clmns.classes)), after = 0L)
  colnames(R.clmns.classes) <- c("NAME", "TYPE")
  return(R.clmns.classes)

  return(data.frame(R.zero.rows))

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

    SchemaAndName <- dbObjectNameFix(conn, o.nm = const.name, as.identifier = TRUE, dbQuote = "Literal")

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

  name <- dbObjectNameFix(conn, o.nm = name)
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
    reference <- dbObjectNameFix(conn, o.nm = reference)
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
  name <- dbObjectNameFix(conn, o.nm = name)
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

  name <- dbObjectNameFix(conn, o.nm = name)
  nameque <- paste(name, collapse = ".")
  colname <- DBI::dbQuoteIdentifier(conn, colname)
  action <- toupper(match.arg(action))
  args <- ifelse(action == "ADD", coltype, ifelse(cascade,
                                                  "CASCADE", ""))
  tmp.query <- paste0("ALTER TABLE ", nameque, " ", toupper(action),
                      " COLUMN ", colname, " ", args, ";")
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }
  if (exec) {
    dbExecuteEM(conn, Statement = tmp.query, display = display, exec = exec)
  }
  if (exec)
    return(TRUE)
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

  name <- dbObjectNameFix(conn, o.nm = name)
  nameque <- paste(name, collapse = ".")

  partition <- dbObjectNameFix(conn, o.nm = partition)
  partitionque <- paste(partition, collapse = ".")

  if(if.not.exists) {
     Results <- dbListInheritEM(conn, name = name)
     if(!NROW(Results)) {
       message(past0("Partitioned ", toupper(dbobject), " ", paste0(nameque, collapse = "."), " does not exist, so skipping . . ."))
       return(invisible(TRUE))
     }
  }

  if(if.not.exists) {
    Results <- dbListInheritEM(conn, name = partition)
    if(!NROW(Results)) {
      message(past0("Target ", toupper(dbobject), " ", paste0(partitionque, collapse = "."), " does not exist, so skipping . . ."))
      return(invisible(TRUE))
    }
  }

  if(if.not.exists) {
    Results <- dbListInheritEM(conn, name = name)
    if(1) {
      MatchSchemaIdx <- match(noquote(paste0(first(nameque))) %in% Results$CHILD_SCHEMA)
      MatchNameIdx   <- match(noquote(paste0(last(nameque)))  %in% Results$CHILD)
      if(length(match(MatchSchemaIdx %in% MatchNameIdx))) {
         message("Partition object with Partition, relationship already exists, so skipping ...")
        return(invisible(TRUE))
      }
    }
  }

  action <- toupper(match.arg(action))
  args <- ifelse(action == "ATTACH" && dbobject == "table", paste0(" FOR VALUES IN (" , part.bound.value , ") "), "")

  tmp.query <- paste0("ALTER ", toupper(dbobject) , " ", nameque, " ", action,
                      " PARTITION ", partitionque, " ", args, ";")

  dbExecuteEM(conn, Statement = tmp.query, display = display, exec = exec)

  if (exec)
    return(TRUE)
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
#' @importFrom DBI dbListFields dbGetQuery dbSendQuery dbColumnInfo dbClearResult
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

  # table does not exist
  # 1. create partitioned table ("p" - partitioned table) (c1)
  #
  if(!DBI::dbExistsTable(conn, name = DfName)) {

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

        # alter table add constraint x primary key(default)
        dbAddKeyEM(conn, name = DfPartName, colname = PrimaryKey, const.name = paste0(DfPartName, "_pk"),
                   display = TRUE, exec = TRUE)

        # alter table add constraint x check
        dbAddKeyEM(conn, name = DfPartName, colname = PartKeyCol, const.name = paste0(DfPartName, "_chk"),
                   type = "check", check.by = DfPartBoundValue,
                   display = TRUE, exec = TRUE)

        # DfPart: data to be loaded
        dbdfMatchColsEM(conn, name = DfName, value = DfPart)

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

      # alter table add constraint x primary key(default)
      dbAddKeyEM(conn, name = DfPartName, colname = PrimaryKey, if.not.exists = TRUE,
                 const.name = paste0(DfPartName, "_pk"),
                 display = TRUE, exec = TRUE)

      # alter table add constraint x check
      dbAddKeyEM(conn, name = DfPartName, colname = PartKeyCol, if.not.exists = TRUE,
                 const.name = paste0(DfPartName, "_chk"),
                 type = "check", check.by = DfPartBoundValue,
                 display = TRUE, exec = TRUE)


      # DfPart: data to be loaded
      dbdfMatchColsEM(conn, name = DfName, value = DfPart)

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

  # Left over from the R CRAN package Caroline functon dbWriteTable2
  fields <- DBI::dbListFields(conn, name = name)
  fields <- fields[!grepl('\\.\\.pg\\.dropped',fields)]

  ## look for unloadable columns in the df
  colnames(Df) <- tolower(colnames(Df))
  colnames(Df) <- gsub("[.]",'_',colnames(Df))

  # add columns to the data.frame (Df),
  # that exist on the server table (name),
  # but do not exist in the data.frame (Df)

  ColsToAddtoDfIndex <-
    !dbRClmnsClassesEM(conn, name = name, temporary = temporary)$NAME  %in%
     dfServerFieldsClassesEM(conn, Df = Df)$NAME

  as.Date <- zoo::as.Date
  mapply(function(Name, Type) {
    Df[[Name]] <- rep(NA, NROW(Df))
    DF[[Name]] <- eval(parse(text = paste0("as.", Type, "(DF[[Name]])")))
    invisible()
  },
  dbRClmnsClassesEM(conn, name = name, temporary = temporary)$NAME[ColsToAddtoDfIndex],
  dbRClmnsClassesEM(conn, name = name, temporary = temporary)$TYPE[ColsToAddtoDfIndex],
  SIMPLIFY = FALSE)

  # add columns to the server table (name),
  # that exist on the data.frame (Df),
  # but do not exist in the server table (name)

  ColsToAddtoServerIndex <-
    !dfServerFieldsClassesEM(conn, Df = Df)$NAME %in%
     dbRClmnsClassesEM(conn, name = name)$NAME

  mapply(function(Name, Type) {
    dbColumnEM(conn, name = name, colname = Name, coltype = Type, display = display, exec = exec)
    invisible()
  },
  dfServerFieldsClassesEM(conn, Df = mtcars2s)$NAME[ColsToAddtoServerIndex],
  dfServerFieldsClassesEM(conn, Df = mtcars2s)$TYPE[ColsToAddtoServerIndex],
  SIMPLIFY = FALSE)

  # in the data.frame, re-order the  columns
  # to match those of the server order
  Df <- Df[, cSort(colnames(Df), InitOrder = dbRClmnsClassesEM(conn, name = "mtcars2s")$NAME)
           , drop = F]

  return(Df)

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}
