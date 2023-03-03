% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ojo_connect.R
\name{ojo_connect}
\alias{ojo_connect}
\title{OJO Connect}
\usage{
ojo_connect(..., .admin = FALSE, .global = rlang::is_interactive())
}
\arguments{
\item{.admin}{A logical value indicating whether to connect to the database as an administrator}

\item{host}{The host name of the database server}

\item{port}{The port number of the database server}

\item{username}{The username to use to connect to the database}

\item{password}{The password to use to connect to the database}

\item{.overwrite}{A logical value indicating whether to overwrite the existing .Renviron file}

\item{.install}{A logical value indicating whether to install the database connection or use it only for the current session}
}
\value{
A database connection object created with `pool::dbPool` and `odbc::odbc`
}
\description{
Connect to the Open Justice Oklahoma database
}
\details{
Opens a connection to the Open Justice Oklahoma database using credentials stored in the .Renviron file.
If no credentials exist, prompts for user, password, and host name and provides instructions to store them for future sessions.
}
\section{Side Effects}{

If either the `.global` argument or `rlang::is_interactive` are `TRUE`, an object named `ojodb` is created in the global environment.
}

\examples{
\dontrun{
ojo_connect()
}
}
\seealso{
ojo_auth()
}