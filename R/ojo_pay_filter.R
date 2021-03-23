#' Filter OSCN fines and fees for inclusion in analysis
#'
#' Filters a data frame of fees from the OJO database using consistent criteria. ojo_fee_filter removes rows that contain certain strings and those that are over $300,000. Requires columns named 'min_desc' and 'fee_amt'.
#'
#' Filter code:
#' filter(!str_detect(pay_acct, "CASH BOND|FORFEIT|JAIL COSTS|HOLDING"),
#'       pay_amt < 2000)
#'
#' @examples
#' \dontrun{
#' ojo_tbl("oscn_pays_2015") %>%
#'    collect() %>%
#'    ojo_pay_filter()
#' }

ojo_pay_filter <- function(df) {

  fdf <- df %>%
    filter(!str_detect(pay_acct, "CASH BOND|FORFEIT|JAIL COSTS|HOLDING"),
           pay_amt < 2000)

  message(scales::comma(nrow(df) - nrow(fdf)), " (", round((nrow(df) - nrow(fdf))/nrow(df)*100, 1), "%) rows removed by ojo_pay_filter()")

  return(fdf)
}

connect_ojo()
df <- ojo_tbl("oscn_pays_2015") %>%
  filter(casetype == "CM", court == "TULSA") %>%
  collect()

fdf <- df %>%
  ojo_pay_filter()
