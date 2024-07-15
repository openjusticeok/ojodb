#' @title OJO Eviction Cases
#'
#' @description
#' Collects Oklahoma Eviction data for the specified districts (or all of OK)
#' for the user specified timeframe. If date_end is not specified, the
#' most up-to-date data will be collected.
#'
#' @param district District code for which to collect data (default is NA, which collects data for all districts)
#' @param date_start Start date for the data collection period
#' @param date_end End date for the data collection period (default is NULL, which collects the most up-to-date data)
#'
#' @importFrom dplyr filter select left_join mutate case_when
#' @importFrom stringr str_detect
#' @importFrom lubridate floor_date as_date
#'
#' @export ojo_eviction_cases
#' @return A dataframe containing eviction data
#'
#' @examples
#' \dontrun{
#' ojo_eviction_cases()
#' ojo_eviction_cases(districts = c("TULSA", "ADAIR"))
#' ojo_eviction_cases(
#'   districts = c("TULSA", "ADAIR"),
#'   date_start = "2020-01-01",
#'   date_end = "2020-12-31",
#'   more_issue_variables = "disposition_date",
#' )
#' ojo_eviction_cases(
#'   districts = c("TULSA", "ADAIR"),
#'   get_judgments = TRUE
#' )
#' }
#'

ojo_eviction_cases <- function(district = "all",
                               ...,
                               date_start = NA,
                               date_end = NA,
                               more_case_variables = NULL,
                               more_issue_variables = NULL,
                               get_judgments = FALSE) {
  #### Variable Handling
  .district <- toupper(district)

  ##### Data Wrangling / Cleaning
  ## Construct Data
  data <- ojo_tbl("case")

  if (.district != "all") {
    data <- data |>
      dplyr::filter(district %in% .district)
  }
  if (!is.na(date_end)) {
    data <- data |>
      dplyr::filter(date_filed <= date_end)
  }
  if (!is.na(date_start)) {
    data <- data |>
      dplyr::filter(date_filed >= date_start)
  }

  data <- data |>
    dplyr::filter(case_type == "SC") |>
    dplyr::select(id, district, date_filed, date_closed, status, more_case_variables) |>
    dplyr::left_join(
      ojo_tbl("issue") |>
        dplyr::select(id, case_id, description, disposition, more_issue_variables),
      by = c("id" = "case_id"),
      suffix = c(".case", ".issue")
    )

  ## Keep Only Eviction Cases
  #   This our standard "strict" definition since it is a more research oriented application.
  data <- data |>
    dplyr::filter(
      stringr::str_detect(
        description,
        "RENT|FORCI|EVICT|DETAIN"
      )
    )

  ## Collect Data from Query
  data <- data |> ojo_collect()

  ## Create Year and Month Filed Variables
  data <- data |>
    dplyr::mutate(
      year_filed = lubridate::floor_date(date_filed, "year") |> lubridate::as_date(),
      month_filed = lubridate::floor_date(date_filed, "month") |> lubridate::as_date()
    )

  if (get_judgments == TRUE) {
    data <- data |>
      dplyr::mutate(clean_disposition = case_when(
        stringr::str_detect(disposition, "DISMISS") ~ "DISMISSED",
        stringr::str_detect(disposition, "JUDGMENT|JUDGEMENT") ~
          case_when(
            stringr::str_detect(disposition, "DEFAULT") ~ "DEFAULT JUDGMENT",
            stringr::str_detect(disposition, "PLAINTIFF") ~ "JUDGMENT FOR PLAINTIFF",
            stringr::str_detect(disposition, "DEFENDANT") ~ "JUDGMENT FOR DEFENDANT",
            TRUE ~ "JUDGMENT ENTERED"
          ),
        stringr::str_detect(disposition, "ADVISEMENT") ~ "UNDER ADVISEMENT"
      ))

    data <- data |>
      dplyr::mutate(judgment = dplyr::case_when(
        clean_disposition %in% c("DEFAULT JUDGMENT", "JUDGMENT FOR PLAINTIFF") ~ "Eviction Granted",
        clean_disposition == "JUDGMENT FOR DEFENDANT" ~ "Eviction Denied",
        clean_disposition == "JUDGMENT ENTERED" ~ "Case Decided, Outcome Unknown",
        clean_disposition == "DISMISSED" ~ "Case Dismissed (Settled Outside Court)",
        clean_disposition == "UNDER ADVISEMENT" ~ "Case Under Advisement",
        .default = "Case Undecided"
      ))
  }

  return(data)
}