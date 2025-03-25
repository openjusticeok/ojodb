#' Query criminal cases from the OJO database
#'
#' Query the Open Justice Oklahoma database for criminal cases with a case type of 'CM' (misdemeanor) or 'CF' (felony)
#'
#' @param districts A character vector of districts to query
#' @param vars A character vector of variables to return
#' @param case_types A character vector of case types to query
#' @param file_years A character vector of years to query
#' @param ... Placeholder for additional arguments
#'
#' @export ojo_crim_cases
#' @return data, a lazy tibble containing the resulting criminal cases
#' @examples
#' \dontrun{
#' ojo_crim_cases()
#' ojo_crim_cases(districts = c("TULSA", "ADAIR"))
#' ojo_crim_cases(vars = "all")
#' ojo_crim_cases(vars = c("updated_at", "created_at"))
#' }
#'
ojo_crim_cases <- function(districts = "all", vars = NULL, case_types = c("CM", "CF", "TR"),
                           file_years = 2000:lubridate::year(Sys.Date()), ...) {

  case_types_upper <- toupper(case_types)

  if (!all(case_types_upper %in% c("CM", "CF", "TR"))) {
    stop("The 'case_types' argument must only include 'CM', 'CF', or 'TR' cases.")
  }

  data <- ojo_tbl("case") |>
    dplyr::filter(
      # `upper()` is evaluated in SQL; debug and use `show_query()` to verify
      .data$case_type %in% case_types_upper,
      .data$year %in% file_years
    )

  if (all(districts != "all")) {

    districts_upper <- toupper(districts)

    data <- data |>
      dplyr::filter(.data$district %in% districts_upper)
  }

  selection <- c("id", "district", "case_number", "case_type", "date_filed", "date_closed", "counts", "open_counts")

  if (is.null(vars)) {
    data <- data |>
      dplyr::select(dplyr::all_of(selection)) |>
      ojo_add_counts()
    return(data)
  } else {
    if (any(vars == "all")) {
      data <- data |>
        ojo_add_counts()
      return(data)
    } else {
      selection <- append(selection, vars) |>
        unique()

      data <- data |>
        dplyr::select(dplyr::all_of(selection)) |>
        ojo_add_counts()

      return(data)
    }
  }
}


#' Add counts to a tibble of cases
#'
#' @param data A tibble returned by an `ojo_` prefixed function
#' @param vars Variable names from the `count` table to include
#' @param ... Placeholder for future arguments
#'
#' @return A lazy tibble with counts for each case
#' @export
#'
#' @examples
#' \dontrun{
#' ojo_crim_cases(vars = c("counts", "open_counts")) |>
#'   ojo_add_counts()
#'}
#'
ojo_add_counts <- function(data, vars = NULL, ...) {
  if (!inherits(data, "tbl_lazy")) {
    stop("Don't use `collect()` before this function")
  }

  columns <- colnames(data)

  if (!all(c("counts", "open_counts") %in% columns)) {
    stop("`data` must have the columns `counts` and `open_counts`")
  }

  counts_data <- data |>
    dplyr::left_join(
      ojo_tbl("count"),
      by = c("id" = "case_id"),
      suffix = c("", ".count")
    )

  open_counts_data <- data |>
    dplyr::mutate(
      count_as_filed = dplyr::sql("unnest(open_counts)"),
      disposition = NULL
    )

  final_data <- dplyr::union_all(counts_data, open_counts_data)

  if (is.null(vars)) {
    final_data <- final_data |>
      dplyr::select(dplyr::all_of(columns), "count_as_filed", "disposition")
  } else {
    if (all(vars != "all")) {
      selection <- c(dplyr::all_of(columns), dplyr::all_of(vars))
      final_data <- final_data |>
        dplyr::select(dplyr::all_of(selection))
    }
  }

  return(final_data)
}
