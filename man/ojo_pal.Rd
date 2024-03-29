% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ojo_colors.R
\docType{data}
\name{ojo_pal}
\alias{ojo_pal}
\title{Add OJO styling to a ggplot}
\format{
An object of class \code{character} of length 8.
}
\usage{
ojo_pal
}
\description{
Add OJO styling to a ggplot
}
\examples{
\dontrun{
ggplot(ojo_example, aes(file_year, n_cases, color = court)) +
  geom_line(size = 1.5) +
  theme_ojo() +
  ojo_color() +
  scale_x_continuous(
    breaks = 2010:2019,
    limits = c(NA, 2019)
  )
}

}
\keyword{datasets}
