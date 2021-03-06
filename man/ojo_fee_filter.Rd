% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ojo_fee_filter.R
\name{ojo_fee_filter}
\alias{ojo_fee_filter}
\title{Filter OSCN fines and fees for inclusion in analysis}
\usage{
ojo_fee_filter(df)
}
\description{
Filters a data frame of fees from the OJO database using consistent criteria. ojo_fee_filter removes rows that contain certain strings and those that are over $300,000. Requires columns named 'min_desc' and 'fee_amt'.
}
\details{
Filter code:
filter(!str_detect(min_desc, "CASH BOND|FORFEIT|WARR(E|A)NT RETUR|JAIL COSTS|CREDIT TIME SERVED|PAID BY DIS|DECEASED|ADJUSTING ENTRY|CASE NOT PROCESSED|AC22|AC36|AC72|SFIJC|TR"), fee_amt < 300000, fee_amt > 0)
}
\examples{
\dontrun{
ojo_tbl("oscn_mins_2015CF") \%>\%
   filter(!is.na(fee_amt)) \%>\%
   collect() \%>\%
   ojo_fee_filter()
}
}
