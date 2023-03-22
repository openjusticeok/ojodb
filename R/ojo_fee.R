#' @title Fee Translation
#'
#' @description Translates the fee codes to their full description
#'
#' @param df A data frame containing the fee data
#'
#' @export
#' @returns A data frame with the fee codes translated to their full description
#'
ojo_fee_translate <- function(df) {

  f <- system.file(
    "extdata",
    "archive",
    "minute_code_reference.csv",
    package = "ojodb"
  )

  t <- readr::read_csv(f)

  df_translated <- df |>
    dplyr::left_join(
      t,
      by = c("code" = "min_code")
    )

  return(df_translated)

}

#' @title Fee Filter
#'
#' @description Filters out fees that are not relevant to the analysis
#'
#' @param df A data frame containing the fee data
#'
#' @export
#' @returns A data frame with the irrelevant fees removed
#'
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
  filter_string_codes <- paste("\\b", filter_code_terms, "\\b", sep = "", collapse = "|")

  fdf <- df |>
    dplyr::filter(
      !stringr::str_detect(description, filter_string_desc),
      !stringr::str_detect(code, filter_string_codes),
      .data$amount < 300000,
      .data$amount > 0
    )

  filtered_results <- df |>
    dplyr::mutate(
      exclusion = dplyr::case_when(
        .data$amount > 300000 ~ "AMOUNT TOO HIGH (> $300,000)",
        stringr::str_detect(description, filter_string_desc) ~ stringr::str_extract(description, filter_string_desc),
        stringr::str_detect(code, filter_string_codes) ~ code,
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::group_by(.data$exclusion) |>
    dplyr::filter(!is.na(.data$exclusion)) |>
    dplyr::summarize(
      rows_filtered = dplyr::n(),
      amt_filtered = sum(amount, na.rm = TRUE)
    )

  message(
    scales::comma(nrow(df) - nrow(fdf)),
    " (",
    round((nrow(df) - nrow(fdf)) / nrow(df) * 100, 1),
    "%) rows removed by ojo_fee_filter()"
  )

  filtered_results |>
    janitor::adorn_totals(where = "row") |>
    print()

  return(fdf)
}


#' @title Payment Filter
#'
#' @description Filters out payments that are not relevant to the analysis
#'
#' @param df A data frame containing the payment data
#'
#' @export
#' @returns A data frame with the irrelevant payments removed
#'
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

  fdf <- df |>
    dplyr::filter(
      !stringr::str_detect(pay_acct, filter_string_desc),
      !str_detect(pay_code, filter_string_codes),
      pay_amt < 2000 | !is.na(adj_amt)
    )

  filtered_results <- df |>
    dplyr::mutate(
      exclusion = dplyr::case_when(
        pay_amt > 2000 ~ "AMOUNT TOO HIGH (> $2,000)",
        stringr::str_detect(pay_acct, filter_string_desc) ~ stringr::str_extract(pay_acct, filter_string_desc),
        stringr::str_detect(pay_code, filter_string_codes) ~ pay_code,
        TRUE ~ NA_character_
      )
    )  |>
    dplyr::group_by(exclusion) |>
    dplyr::filter(!is.na(exclusion)) |>
    dplyr::summarize(
      rows_filtered = dplyr::n(),
      amt_filtered = sum(pay_amt, na.rm = TRUE)
    )

  message(
    scales::comma(nrow(df) - nrow(fdf)),
    " (",
    round((nrow(df) - nrow(fdf)) / nrow(df) * 100, 1),
    "%) rows removed by ojo_pay_filter()"
  )

  filtered_results |>
    janitor::adorn_totals(where = "row") |>
    print()

  return(fdf)
}
