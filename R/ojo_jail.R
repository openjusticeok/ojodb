#' Query arrests from ojodb jail data tables
#'
#' Query the Open Justice Oklahoma database for jail records from Tulsa or Oklahoma County
#'
#' @export ojo_arrests
#' @return data, a lazy tibble containing the resulting jail data
#' @examples
#' \dontrun{
#' ojo_jail_admits()
#' }
#'

ojo_arrests <- function(county = "TULSA",
                        years = 2018:year(Sys.Date()),
                        include_charges = FALSE) {

  message("Querying current and archived schemas . . .")

  if (include_charges == FALSE) {
  data_new <- ojo_tbl("arrest", schema = "iic") |>
    collect()

  data_old <- ojo_tbl("dlm2_inmates", schema = "archive") |>
    filter(year(book_date) %in% years) |>
    collect()
  } else {
      data_new <- ojo_tbl("arrest", schema = "iic") |>
        head(100) |>
        left_join(ojo_tbl("offense", schema = "iic") |>
                    select(-updated_at, -created_at)) |>
        collect()

      data_old <- ojo_tbl("dlm2_inmates", schema = "archive") |>
        filter(year(book_date) %in% years) |>
        collect()
  }

  data <- data_old |>
    mutate(dlm = dlm_id |>
             str_remove("^0*") |>
             as.integer()
    ) |>
    select(id = dlm_arr_id, dlm, date = arrest_date,
           time = arrest_time,
           datetime = arrest_datetime,
           arrested_by = arrest_agency,
           booking_date = book_date,
           booking_time = book_time,
           booking_datetime = book_datetime,
           release_date, release_time, release_datetime) |>
    select(1:12) |>
    arrange(booking_datetime) |>
    filter(!is.na(date)) |>
    bind_rows(data_new |>
                select(1:12))

  return(data)
}


#' Add charges to a tibble of jail admissions
#'
#' @param data A tibble returned by an `ojo_` prefixed function
#' @param vars Variable names from the `count` table to include
#' @param ... Placeholder for future arguments
#'
#' @return A tibble with counts for each case
#' @export
#'
#' @examples
#' ojo_crim_cases(vars = c("counts", "open_counts")) |>
#'   ojo_add_counts()
#'
ojo_add_counts <- function(data, vars = NULL, ...) {
  if(!"tbl_lazy" %in% class(data)) {
    stop("Don't use `collect()` before this function")
  }

  data <- data |>
    mutate(count = unnest(counts),
           open_count = unnest(open_counts)) |>
    left_join(ojo_tbl("count"),
              by = c("count" = "id"),
              suffix = c("", ".count")) |>
    mutate(count_as_filed = if_else(is.na(open_count),
                                    count_as_filed,
                                    open_count))

  if(is.null(vars)) {
    data <- data |>
      select(all_of(columns), count_as_filed)
  } else {
    if(vars != "all") {
      selection <- c(all_of(columns), all_of(vars))
      data <- data |>
        select(all_of(selection))
    }
  }

  return(data)
}
