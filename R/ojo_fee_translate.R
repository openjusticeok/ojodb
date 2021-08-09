#' Translates the OSCN fine / fee codes ('min_code' and 'pay_code') for use in analysis. Adds one column "translating" the fee code and one column with a category (e.g. "Indigent Defense", "Jail Costs", etc.)
#'
#' @examples
#' \dontrun{
#' ojo_tbl("oscn_mins_2015CF") %>%
#'    filter(!is.na(fee_amt)) %>%
#'    collect() %>%
#'    ojo_fee_filter() %>%
#'    ojo_fee_translate()
#' }
#'

ojo_fee_translate <- function(df) {

  t <- read_csv("data/min_code_translations.csv")

  df_translated <- merge(df, t, by = "min_code", all.x = T, sort = F)

  # Changes the column order back to how it was before the merge()
  df_translated <- df_translated[c(2, 3, 4, 5, 6, 7, 8, 1, 9, 10, 11, 12, 13, 14)]

  return(df_translated)

}

