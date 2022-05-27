#' Query minutes for a given case
#'
#' Query the Open Justice Oklahoma database for the minutes of a case
#'
#' @export ojo_parse_account_minute
#' @return data, a lazy tibble containing the resulting cases with minutes
#' @examples
#' \dontrun{
#' ojo_add_minutes()
#' }
#'

ojo_parse_account_minute <- function(data, ...) {

  return(NA)
}

get_account_minute_type <- function(description, ...) {
  if(!is.character(description) | !length(description) == 1) {
    stop("Argument 'description' should be a 'character' of length '1'")
  }

  type <- case_when(
    str_detect(description, "VOUCHER") ~ "voucher",
    str_detect(description, "VOIDED") ~ "voided",
    str_detect(description, "RECEIPT") &
      (!str_detect(description, "REFUND") |
      !str_detect(description, "CHARGEBACK")) ~ "receipt",
    str_detect(description, "ADJUSTING") ~ "adjustment",
    str_detect(description, "DISBURSEMENT") &
      !str_detect(description, "VOUCHER") ~ "disbursement",
    str_detect(description, "REFUND") ~ "refund",
    str_detect(description, "CHARGEBACK") ~ "chargeback",
    !str_detect(description, "RECEIPT|ADJUSTING|DISBURSEMENT|VOUCHER|REFUND|VOIDED|CHARGEBACK") ~ "other",
    TRUE ~ NA_character_
  )

  return(type)
}

test <- ojo_tbl("minute") |>
  filter(code == "ACCOUNT") |>
  select(description) |>
  head(n = 10000) |>
  collect() |>
  mutate(
    type = map_chr(description, get_account_minute_type)
  )

test |>
  count(type)
