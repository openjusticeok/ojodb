#' Query arrests from ojodb jail data tables
#'
#' Query the Open Justice Oklahoma database for jail records from Tulsa County.
#'
#' @export ojo_arrests
#' @return data, a lazy tibble containing the resulting jail data
#' @examples
#' \dontrun{
#' ojo_arrests()
#' ojo_arrests(years = 2019:2022,
#'             include_charges = TRUE)
#' }
#'

ojo_arrests <- function(county = "TULSA",
                        years = 2018:year(Sys.Date()),
                        include_charges = FALSE) {

  message("Querying current and archived schemas . . .")

  tictoc::tic()
  data_new <- ojo_tbl("arrest", schema = "iic") |>
    filter(year(booking_date) %in% years,
           !is.na(date)) |>
    select(-updated_at)

  # BASED ON dlm2 TABLES
  data_old <- ojo_tbl("dlm2_inmates", schema = "archive") |>
    select(id = dlm_arr_id,
           dlm = dlm_id,
           date = arrest_date,
           time = arrest_time,
           datetime = arrest_datetime,
           arrested_by = arrest_agency,
           booking_date = book_date,
           booking_time = book_time,
           booking_datetime = book_datetime,
           release_date, release_time,
           release_datetime) |>
    filter(year(booking_date) %in% years)

  if (include_charges == TRUE) {
    data_new_c <- data_new |>
      left_join(ojo_tbl("offense", schema = "iic") |>
                  rename(offense_id = id),
                by = c("dlm", "date" = "arrest_date")) |>
      group_by(booking_id) |>
      filter(updated_at == max(updated_at)) |>
      ungroup() |>
      select(1:12, 19:26, 28) |>
      collect() |>
      mutate(rank = str_extract(offense_id, '(?<=rank": )\\d') |>
               as.numeric()) |>
      filter(rank == 0) |>
      select(-rank, -offense_id, -case_uri, -updated_at) |>
      rename(offense = description)

    # Add charges to old data and collect
    data_old_c <- data_old |>
      left_join(ojo_tbl("dlm2_offenses", schema = "archive"),
                by = c("dlm" = "dlm_id")) |>
      rename(offense = off_desc,
             case_number = casenum,
             court_date = next_date,
             bond_amount = bond_amt) |>
      collect()

    # Group by variables in inmate table plus offense description
    dots <- names(data_old_c[c(1:12, 16)]) |>
      lapply(as.name)

    # Calculate days from arrest to court date
    data_old_a <- data_old_c |>
      mutate(days_to_court = as.numeric(court_date) - as.numeric(booking_date)) |>
      ungroup() |>
      group_by(.dots = dots) |>
      summarize(case_number = first(case_number[which(!is.na(case_number) & case_number != "")]),
                min_days = min(days_to_court, na.rm = TRUE),
                bond_amount = min(bond_amount[which(!is.na(bond_amount))]),
                bond_type = first(bond_type[which(!is.na(bond_type))]),
                .groups = "keep")

    # Narrow down to charges with a court_date within 90 days after arrest date
    data_old_b <- data_old_a |>
      filter(min_days <= 90,
             min_days >= 0) |>
      mutate(bond_amount = if_else(bond_amount == Inf,
                                   as.numeric(NA),
                                   bond_amount))

    data_old_d <- data_old_a |>
      ungroup() |>
      anti_join(data_old_b |>
                  ungroup() |>
                  select(id),
                by = "id") |>
      group_by(.dots = names(data_old_a[1:13]) |>
                 lapply(as.name)) |>
      filter(min_days == max(min_days))

    data_old_f <- data_old_b |>
      bind_rows(data_old_d) |>
      arrange(booking_date, dlm)

    data_old <- data_old_f
    data_new <- data_new_c
  } else {

    data_new <- collect(data_new)
    data_old <- collect(data_old)

  }

  data <- data_old |>
    mutate(dlm = str_remove(dlm, "^0+") |>
             as.numeric()) |>
    bind_rows(data_new) |>
    arrange(booking_date) |>
    filter(year(booking_date) %in% years) |>
    distinct()


  tictoc::toc()
  return(data)
}
