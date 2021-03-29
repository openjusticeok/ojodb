#' Filter OSCN fines and fees for inclusion in analysis
#'
#' Filters a data frame of fees from the OJO database using consistent criteria. ojo_fee_filter removes rows that contain certain strings and those that are over $300,000. Requires columns named 'min_desc' and 'fee_amt'.
#'
#' Filter code:
#' filter(!str_detect(min_desc, "CASH BOND|FORFEIT|WARR(E|A)NT RETUR|JAIL COSTS|CREDIT TIME SERVED|PAID BY DIS|DECEASED|ADJUSTING ENTRY|CASE NOT PROCESSED"), fee_amt < 300000)
#'
#' @examples
#' \dontrun{
#' ojo_tbl("oscn_mins_2015CF") %>%
#'    filter(!is.na(fee_amt)) %>%
#'    collect() %>%
#'    ojo_fee_filter() %>%
#' }

ojo_fee_filter <- function(df) {

  fdf <- df %>%
    filter(!str_detect(min_desc, "CASH BOND|FORFEIT|WARR(E|A)NT RETUR|JAIL COSTS|CREDIT TIME SERVED|PAID BY DIS|DECEASED|ADJUSTING ENTRY|CASE NOT PROCESSED"),
           fee_amt < 300000)

  message(scales::comma(nrow(df) - nrow(fdf)), " (", round((nrow(df) - nrow(fdf))/nrow(df)*100, 1), "%) rows removed by ojo_fee_filter()")

  return(fdf)
}