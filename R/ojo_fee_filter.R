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

  filter_desc_terms <- c("CASH BOND",
                    "FORFEIT",
                    "WARR(E|A)NT RETUR",
                    "JAIL COSTS",
                    "CREDIT TIME SERVED",
                    "PAID BY DIS",
                    "DECEASED",
                    "ADJUSTING ENTRY",
                    "CASE NOT PROCESSED")
  
  filter_code_terms <- c("AC22", 
                         "AC35", 
                         "AC72", 
                         "SFIJC", 
                         "TR")

  filter_string_desc <- paste(filter_desc_terms, collapse = "|")
  filter_string_codes <- paste(filter_code_terms, collapse = "|")

  fdf <- df %>%
    filter(!str_detect(min_desc, filter_string_desc),
           !str_detect(min_code, filter_string_codes),
           fee_amt < 300000,
           fee_amt > 0)

  filtered_results <- df %>%
    mutate(exclusion = if_else(fee_amt > 300000, "AMOUNT TOO HIGH (> $300,000)",
                               ifelse(fee_amt < 0, "AMOUNT TOO LOW (< $0)",
                                      ifelse(str_detect(min_desc, filter_string_desc), str_extract(min_desc, filter_string_desc),
                                             ifelse(str_detect(min_code, filter_string_codes), min_code, NA))))) %>%
                                 
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
