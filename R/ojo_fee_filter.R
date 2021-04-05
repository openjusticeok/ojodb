#' Filter OSCN fines and fees for inclusion in analysis
#'
#' Filters a data frame of fees from the OJO database using consistent criteria. ojo_fee_filter removes rows that contain certain strings and those that are over $300,000. Requires columns named 'min_desc' and 'fee_amt'.
#'
#' Filter code:
#' filter(!str_detect(min_desc, "CASH BOND|FORFEIT|WARR(E|A)NT RETUR|JAIL COSTS|CREDIT TIME SERVED|PAID BY DIS|DECEASED|ADJUSTING ENTRY|CASE NOT PROCESSED|AC22|AC36|AC72|SFIJC|TR"), fee_amt < 300000, fee_amt > 0)
#'
#' @examples
#' \dontrun{
#' ojo_tbl("oscn_mins_2015CF") %>%
#'    filter(!is.na(fee_amt)) %>%
#'    collect() %>%
#'    ojo_fee_filter()
#' }

ojo_fee_filter <- function(df) {

  filter_terms <- c("CASH BOND",
                    "FORFEIT",
                    "WARR(E|A)NT RETUR",
                    "JAIL COSTS",
                    "CREDIT TIME SERVED",
                    "PAID BY DIS",
                    "DECEASED",
                    "ADJUSTING ENTRY",
                    "CASE NOT PROCESSED",
                    "AC22",               # Incarceration costs
                    "AC35",               # Incarceration costs
                    "AC72",               # Incarceration costs (split between Sheriff and DA in Pushmataha)
                    "SFIJC",              # Incarceration costs
                    "TR")                 # Account transfers

  filter_string <- paste(filter_terms, collapse = "|")

  fdf <- df %>%
    filter(!str_detect(min_desc, filter_string),
           fee_amt < 300000,
           fee_amt > 0)

  filtered_results <- df %>%
    mutate(exclusion = if_else(fee_amt > 300000,
                               "AMOUNT OVER $300k",
                               str_extract(min_desc, filter_string))) %>%
    group_by(exclusion) %>%
    filter(!is.na(exclusion)) %>%
    summarize(rows_filtered = n(),
              amt_filtered = sum(fee_amt, na.rm = TRUE))

  message(scales::comma(nrow(df) - nrow(fdf)), " (", round((nrow(df) - nrow(fdf))/nrow(df)*100, 1), "%) rows removed by ojo_fee_filter()")

  filtered_results %>%
    adorn_totals(where = "row") %>%
    print()

  return(fdf)
}
