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

# Old filter:
# ojo_pay_filter <- function(df) {
# 
#   fdf <- df %>%
#     filter(!str_detect(pay_acct, "CASH BOND|FORFEIT|JAIL COSTS|HOLDING"),
#            pay_amt < 2000 | !is.na(adj_amt))
# 
#   message(scales::comma(nrow(df) - nrow(fdf)), " (", round((nrow(df) - nrow(fdf))/nrow(df)*100, 1), "%) rows removed by ojo_pay_filter()")
# 
#   return(fdf)
# }

# - New Filter ----------------

ojo_pay_filter <- function(df) {
  
  filter_desc_terms <- c("CASH BOND",
                         "FORFEIT",
                         "HOLDING",
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
  filter_string_codes <- paste("\\b", filter_code_terms, "\\b", sep = "", collapse = "|")
  
  fdf <- df %>%
    filter(!str_detect(pay_acct, filter_string_desc),
           !str_detect(pay_code, filter_string_codes),
           pay_amt < 2000 | !is.na(adj_amt))
  
  filtered_results <- df %>%
    mutate(exclusion = case_when(pay_amt > 2000 ~ "AMOUNT TOO HIGH (> $2,000)",
                                 str_detect(pay_acct, filter_string_desc) ~ str_extract(pay_acct, filter_string_desc),
                                 str_detect(pay_code, filter_string_codes) ~ pay_code,
                                 TRUE ~ as.character(NA)))  %>%
    group_by(exclusion) %>%
    filter(!is.na(exclusion)) %>%
    summarize(rows_filtered = n(),
              amt_filtered = sum(pay_amt, na.rm = TRUE))
  
  message(scales::comma(nrow(df) - nrow(fdf)), " (", round((nrow(df) - nrow(fdf))/nrow(df)*100, 1), "%) rows removed by ojo_pay_filter()")
  
  filtered_results %>%
    adorn_totals(where = "row") %>%
    print()
  
  return(fdf)
}
