library(ojodb)
library(dplyr)
library(ggplot2)
library(duckdb)
library(arrow)
library(googleCloudStorageR)
library(purrr)
library(stringr)

#' The analyst wants to move fast and not wait
#' - Prioritize speed and fewer network transactions
#' We want to not spend money
#' - Modest internet connection speed, ~ 50 Mbps
#' - Pushing compute to analysts' laptops since they are already paid for
#' - Living with modest memory (~16GB), but ample storage (>500GB)
#'

con <- dbConnect(duckdb())

gcs_auth(json_file = "~/.config/ojo/ojo-database-26e180c16c0f.json")

gcs_list_objects("oscn") |>
  pull(name) |>
  purrr::walk(
    ~ gcs_get_object(
      .x,
      bucket = "oscn",
      saveToDisk = str_glue("~/.cache/ojo/oscn/{.x}"),
      overwrite = TRUE
    )
  )

# Loads to memory
ojo_tbl("case", .source = "gcs", .cache = F) |>
  to_duckdb(
    con,
    table_name = "case"
  )

# Loads to memory
# Doable w/ laptop memory
ojo_tbl("case", .source = "gcs", .cache = F) |>
  arrow::write_dataset(
    "~/.cache/ojo/oscn/case/",
    format = "arrow",
    partitioning = "case_type",
  )

# Loads to memory
# Way too big for laptop memory
ojo_tbl("minute", .source = "gcs", .cache = F) |>
  arrow::write_dataset(
    "~/.cache/ojo/oscn/minute/",
    format = "arrow",
    partitioning = "code"
  )

fs::file_size("~/.cache/ojo/oscn/case/")

# Loads aggregate
# Cowplot the plots by different resolutions of date_filed using lubridate::floor_date()
arrow::open_dataset("~/.cache/ojo/oscn/case", format = "parquet") |>
  filter(district == "TULSA", case_type == "CF") |>
  summarise(
    .by = c("case_type", "district", "date_filed"),
    n = n()
  ) |>
  collect()
  # |>
  # ggplot(aes(x = date_filed, y = n)) +
  #   geom_line()


data <- arrow::open_dataset("~/.cache/ojo/oscn/case", format = "arrow") |>
  arrow::to_duckdb(table_name = "case")

duckdb::dbWriteTable(conn = con, name = "case", value = data)

arrow::open_dataset("~/.cache/ojo/oscn/minute", format = "parquet") |>
  summarise(
    .by = c("code"),
    n = n(),
    total = sum(amount, na.rm = TRUE)
  ) |>
  collect()
