#' Query minutes for a given case
#'
#' Query the Open Justice Oklahoma database for the minutes of a case
#'
#' @export
#' @return data, a lazy tibble containing the resulting cases with minutes
#' @examples
#' \dontrun{
#' ojo_add_minutes()
#' }
#'
ojo_parse_account_minutes <- function(data, ..., .parallel = F, .progress = T) {
  if(.parallel) {
    data <- data |>
      dplyr::mutate(
        type = furrr::future_map_chr(description, extract_account_minute_type, .progress = .progress),
        total_amount_paid = case_when(
          type == "receipt" ~ future_map_chr(description, extract_receipt_total_amount_paid, .progress = .progress) |>
            as.numeric(),
          TRUE ~ NA_real_
        )
      )
  } else {
    data <- data |>
      mutate(
        type = map_chr(description, extract_account_minute_type),
        total_amount_paid = map_chr(description, extract_receipt_total_amount_paid) |>
          as.numeric()
      )
  }

  return(data)
}

extract_account_minute_type <- function(description, ...) {
  if(!is.character(description) | !length(description) == 1) {
    stop("Argument 'description' should be a 'character' of length '1'")
  }

  type <- case_when(
    str_detect(description, "REFUND") ~ "refund",
    str_detect(description, "VOUCHER") ~ "voucher",
    str_detect(description, "VOIDED") ~ "voided",
    str_detect(description, "RECEIPT") &
      (!str_detect(description, "REFUND") &
      !str_detect(description, "CHARGEBACK")) ~ "receipt",
    str_detect(description, "ADJUSTING") ~ "adjustment",
    str_detect(description, "DISBURSEMENT") &
      !str_detect(description, "VOUCHER") ~ "disbursement",
    str_detect(description, "CHARGEBACK") ~ "chargeback",
    !str_detect(description, "RECEIPT|ADJUSTING|DISBURSEMENT|VOUCHER|REFUND|VOIDED|CHARGEBACK") ~ "other",
    TRUE ~ NA_character_
  )

  return(type)
}

extract_receipt_number <- function(description, ...) {
  if(!is.character(description) | !length(description) == 1) {
    stop("Argument 'description' should be a 'character' of length '1'")
  }

  receipt_number <- str_extract(description, "(?<=RECEIPT # ).*?(?= ON)")

  return(receipt_number)
}

extract_receipt_total_amount_paid <- function(description, ...) {
  if(!is.character(description) | !length(description) == 1) {
    stop("Argument 'description' should be a 'character' of length '1'")
  }

  total_amount_paid <- str_extract(description, "(?<=TOTAL AMOUNT PAID:[\\s]?\\$[\\s]?)[\\-]?[\\d\\,]*.[\\d]*(?=\\.[\\s]?(LINE)?)") |>
    str_remove_all(",|\\s")

  return(total_amount_paid)
}
