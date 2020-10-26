
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
#'
#' # examples of
#' # getAAIISIProDate(Data version of the SIPro)
#'
#' getAAIISIProDate()
#' [1] "18535"
#'
#' zoo::as.Date(as.integer(getAAIISIProDate()))
#' [1] "2020-09-30"
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom foreign read.dbf
#' @export
getAAIISIProDate <- function(
  From = "C:/Program Files (x86)/Stock Investor/Professional") {
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

})}








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
#'
#' # copyAAIISIProDBFs(Copy SIPro .DBF Files to a Target Directory) example
#'
#' copyAAIISIProDBFs(
#'     From = "C:/Program Files (x86)/Stock Investor/Professional",
#'     To   = paste0(tempdir(),"/", getAAIISIProDate()),
#'     CaseChange = "UpperCase"
#' )
#' dir(paste0(tempdir(),"/", getAAIISIProDate()))
#' }
#' @importFrom tryCatchLog tryCatchLog
copyAAIISIProDBFs <- function(From = "C:/Program Files (x86)/Stock Investor/Professional",
                              To = tempdir(),
                              CaseChange = "UpperCase") {
tryCatchLog::tryCatchLog({

  SubDirs <- c("","/Dbfs","/User","/Static","/Temp","/Datadict")

  for(SubDir in SubDirs) {

    # it DOES not overwrite what is ALREADY there
    R.utils__copyDirectoryByPattern(from = paste0(From, SubDir),
                                    pattern = "(*\\.dbf$|\\.*DBF$|\\.*DBF$|*.chm$|ReadMe\\.txt)",
                                    to = To, CaseChange = CaseChange
    )

  }

})}


