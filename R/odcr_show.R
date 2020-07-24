odcr_show <- function(court, casetype, year, caseseq) {
  u <- court_ref[court_ref$court == court, "url_pattern"] %>%
    str_replace("XX", paste0(casetype, "+")) %>%
    str_replace("YY", str_sub(year, 3, 4)) %>%
    str_replace("ZZZZ", str_pad(caseseq, side = "left", pad = 0, width = 4))
  browseURL(u)
}
