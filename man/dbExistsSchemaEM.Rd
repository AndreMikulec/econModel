% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AmerAssocIndividInvestorsAAII.R
\name{dbExistsSchemaEM}
\alias{dbExistsSchemaEM}
\title{Schema Existence?}
\usage{
dbExistsSchemaEM(connName, schema, env, display = TRUE, exec = TRUE)
}
\arguments{
\item{connName}{String.  Default is "connEM". Contains the name of the variable that contains the name of the "connection" in the environment "env".}

\item{schema}{String. Required.  Potential schema in the database.}

\item{env}{Environment.  Default is the .Global environment.  This is the environment to return the connection object "connEM".}

\item{display}{Logical. Whether to display the query (defaults to \code{TRUE}).}

\item{exec}{Logical. Whether to execute the query (defaults to \code{TRUE}).}
}
\value{
TRUE(exists) or FALSE(not exists)
}
\description{
Determine if a schema exists in the database.
}
\examples{
\dontrun{
 dbExistsSchemaEM(schema = "r_user")
}
}
