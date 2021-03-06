% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AmerAssocIndividInvestorsAAII.R
\name{fileExt}
\alias{fileExt}
\title{File Extension}
\usage{
fileExt(SubPathFileExt, PathReq = TRUE, ...)
}
\arguments{
\item{SubPathFileExt}{String. Path to the file including FileName and its (after the dot ".") extension.}

\item{PathReq}{Logical. Send SubPathFileExt through normalizePath.}

\item{...}{Dots passed.}

\item{Separ}{String. Regular expression separating the File Extension from its FileName.}
}
\value{
String.
}
\description{
Get the File Extension.
}
\examples{
\dontrun{
fileExt(c(
  "C:\\\\Program Files (x86)\\\\Stock Investor\\\\Professional\\\\setup.dbf",
  "C:\\\\Program Files (x86)\\\\Stock Investor\\\\Professional\\\\Static\\\\si_isq.dbf"
))
}
}
