% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ojo_civ_cases.R
\name{ojo_add_issues}
\alias{ojo_add_issues}
\title{Add issues to civil cases}
\usage{
ojo_add_issues(data, vars = NULL, ...)
}
\arguments{
\item{data}{A lazy tibble of civil cases}

\item{vars}{A character vector of variables to return}

\item{...}{Placeholder for additional arguments}
}
\value{
data, a lazy tibble containing the resulting civil cases
}
\description{
Add issues to civil cases
}
\examples{
\dontrun{
ojo_civ_cases() |>
 ojo_add_issues()
}

}
