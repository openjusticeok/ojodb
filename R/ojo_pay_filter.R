#' Filter OSCN fines and fees for inclusion in analysis
#'
#' Filters a data frame of payments from the OJO database using consistent criteria. ojo_pay_filter removes rows that contain certain strings and those that are over $2,000. Requires columns named 'pay_acct' and 'pay_amt'.
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
           pay_amt < 2000 | !is.na(adj_amt))

  message(scales::comma(nrow(df) - nrow(fdf)), " (", round((nrow(df) - nrow(fdf))/nrow(df)*100, 1), "%) rows removed by ojo_pay_filter()")

  return(fdf)
}
