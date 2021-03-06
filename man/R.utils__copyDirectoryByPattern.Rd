% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/econModelSupport.R
\name{R.utils__copyDirectoryByPattern}
\alias{R.utils__copyDirectoryByPattern}
\title{copy Files and Directories Using a Regular Expression}
\usage{
R.utils__copyDirectoryByPattern(
  from,
  to = ".",
  ...,
  private = T,
  recursive = T,
  pattern = NULL,
  CaseChange = NULL
)
}
\arguments{
\item{from}{The pathname of the source directory to be copied.}

\item{to}{The pathname of the destination directory.}

\item{...}{Additional arguments passed to file.copy(), e.g. overwrite.}

\item{private}{If TRUE, files (and directories) starting with a period is also copied, otherwise not.}

\item{recursive}{If TRUE, subdirectories are copied too, otherwise not.  Note, the name of the subdirectory also must in in "pattern".}

\item{pattern}{regular expression of the names of the source files}

\item{CaseChange}{string. Default is NULL(no change).  Change to target file name to the desired case: NULL(no change), "UpperCase", "LowerCase".}
}
\description{
\preformatted{
Code copy of R.utils::copyDirectory.default with the addition of the
features pattern(source files) CaseChange(destination files).
The function is renamed to be R.utils__copyDirectoryByPattern
}
}
\examples{
\dontrun{

# R.utils__copyDirectoryByPattern example

R.utils__copyDirectoryByPattern("C:/Program Files (x86)/Stock Investor/Professional",
  to = tempdir(), pattern = "(*\\\\.dbf$|\\\\.*DBF$|\\\\.*DBF$|*.chm$|ReadMe\\\\.txt)",
  CaseChange = "UpperCase"
)
dir(tempdir())
}
}
