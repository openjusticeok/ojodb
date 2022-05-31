# Initialize testing data
data <- ojo_tbl("minute") |>
  filter(code == "ACCOUNT") |>
  select(description) |>
  collect()

test_that("categorization works", {
  expect_error(
    data |>
      mutate(
        type = map_chr(description, extract_account_minute_type)
      ),
    NA
  )
})
# data |>
#   count(type)

test_that("can get receipt number", {
  expect_error(
    data |>
      mutate(
        type = map_chr(description, extract_account_minute_type)
      ) |>
      filter(type == "receipt") |>
      mutate(
        receipt_number = map_chr(description, extract_receipt_number)
      ),
    NA
  )
})

test_that("all receipt numbers match format", {
  expect_false(
    data |>
      mutate(
        type = map_chr(description, extract_account_minute_type)
      ) |>
      filter(type == "receipt") |>
      mutate(
        receipt_number = map_chr(description, extract_receipt_number)
      ) |>
      mutate(
        format_test = str_detect(receipt_number, "\\d{4}-\\d{5,7}")
      ) |>
      count(format_test) |>
      pull(format_test) |>
      is.na() |>
      any()
  )
})
# data |>
#   filter(type == "receipt") |>
#   mutate(
#     receipt_number = str_extract(description, "(?<=RECEIPT # ).*?(?= ON)"),
#     format_test = str_detect(receipt_number, "\\d{4}-\\d{5,7}")
#   ) |>
#   filter(
#     !format_test
#   )

test_that("can get total amount from receipt", {
  expect_error(
    data |>
      mutate(
        type = map_chr(description, extract_account_minute_type)
      ) |>
      filter(type == "receipt") |>
      mutate(
        total_amount_paid = map_chr(description, extract_receipt_total_amount_paid)
      ),
    NA
  )
})

test_that("all receipt totals are coercible to numeric", {
  expect_false(
    data |>
      mutate(
        type = map_chr(description, extract_account_minute_type)
      ) |>
      filter(type == "receipt") |>
      mutate(
        total_amount_paid = map_chr(description, extract_receipt_total_amount_paid)
      ) |>
      mutate(
        format_test = as.numeric(total_amount_paid)
      ) |>
      count(format_test) |>
      pull(format_test) |>
      is.na() |>
      any()
  )
})
## View number of uncoercible records
# data |>
#   mutate(
#     type = map_chr(description, extract_account_minute_type)
#   ) |>
#   filter(type == "receipt") |>
#   mutate(
#     total_amount_paid = map_chr(description, extract_receipt_total_amount_paid)
#   ) |>
#   count(is.na(as.numeric(total_amount_paid)))

## View records which were uncoercible
data |>
  mutate(
    type = map_chr(description, extract_account_minute_type)
  ) |>
  filter(type == "receipt") |>
  mutate(
    total_amount_paid = map_chr(description, extract_receipt_total_amount_paid)
  ) |>
  filter(is.na(as.numeric(total_amount_paid)))


# ## Reciept Line Items
#
# test |>
#   filter(type == "receipt") |>
#   mutate(
#     line_item = str_extract(description, "(?<=LINE ITEMS:).*") |>
#       str_extract_all("(?<=\\.\\s)(\\w{2}-\\d{4}-[\\d\\w]*).*(?=\\w{2}-\\d{4}-[\\d\\w]*)")
#   ) |>
#   unnest(cols = "line_item")
#
#
# ## Line Item Amounts
# test |>
#   filter(type == "receipt") |>
#   mutate(
#     line_item = str_extract(description, "(?<=LINE ITEMS:).*") |>
#       str_extract_all("(?<=\\.\\s)(\\w{2}-\\d{4}-[\\d\\w]*).*(?=\\w{2}-\\d{4}-[\\d\\w]*)")
#   ) |>
#   unnest(cols = "line_item") |>
#   mutate(
#     amount = str_match_all(line_item, "(?<=\\$)[\\d\\-\\,\\.]*")
#   ) |>
#   unnest(cols = amount) |>
#   mutate(
#     format_check = str_detect(amount, "[\\d\\,]*\\.\\d{2}")
#   ) |>
#   mutate(amount = as.numeric(str_remove(amount, ",")))
