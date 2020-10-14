#' Pull up a case docket record on the web
#'
#' Pulls up a OSCN or ODCR case docket record in the default web browser. This is useful for investigating individual cases for the ways in which items of interest are recorded in online court records.
#'
#' @param courts A character vector of the courts to scrape, for example, "TULSA" or c("TULSA", "ROGERS")
#' @param casetypes A character vector of the case types to scrape, for example, "CF" or c("CF", "CM")
#' @param years A numeric vector of years to scrape, for example, 2016 or 2015:2017
#' @param case_seq Sequential case numbers to scrape (i.e., the "45" in "CF-2015-45"), for example, 1 or 1:7000
#' @examples
#' \dontrun{
#' odcr_show("TULSA", "CF", 2020, 1)
#' }

odcr_show <- function(court, casetype, year, case_seq) {
  u <- court_ref[court_ref$court == court, "url_pattern"] %>%
    str_replace("XX", paste0(casetype, "+")) %>%
    str_replace("YY", str_sub(year, 3, 4)) %>%
    str_replace("ZZZZ", str_pad(caseseq, side = "left", pad = 0, width = 4))
  browseURL(u)
}
