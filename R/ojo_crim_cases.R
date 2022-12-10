#' Query criminal cases from ojodb
#'
#' Query the Open Justice Oklahoma database for criminal cases with a casetype of'CM' (misdemeanor) or 'CF' (felony)
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
  data <- ojo_tbl("case") |>
    dplyr::filter(
      case_type %in% case_types,
      year %in% file_years
    )

  if (all(districts != "all")) {
    data <- data |>
      dplyr::filter(district %in% districts)
  }

  data <- data |>
    ojo_add_counts()

  selection <- c("id", "district", "case_number", "case_type", "date_filed", "date_closed", "count_as_filed")

  if (is.null(vars)) {
    data <- data |>
      dplyr::select(dplyr::all_of(selection))
    return(data)
  } else {
    if (any(vars == "all")) {
      return(data)
    } else {
      selection <- append(selection, vars) |>
        unique()

      data <- data |>
        dplyr::select(dplyr::all_of(selection))

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
#' @return A tibble with counts for each case
#' @export
#'
#' @examples
#' ojo_crim_cases(vars = c("counts", "open_counts")) |>
#'   ojo_add_counts()
#'
ojo_add_counts <- function(data, vars = NULL, ...) {
  if (!"tbl_lazy" %in% class(data)) {
    stop("Don't use `collect()` before this function")
  }

  columns <- colnames(data)

  if (!all(c("counts", "open_counts") %in% columns)) {
    stop("`data` must have the columns `counts` and `open_counts`")
  }

  data <- data |>
    dplyr::mutate(
      count = tidyr::unnest(counts),
      open_count = tidyr::unnest(open_counts)
    ) |>
    dplyr::left_join(ojo_tbl("count"),
      by = c("count" = "id"),
      suffix = c("", ".count")
    ) |>
    dplyr::mutate(count_as_filed = dplyr::if_else(is.na(open_count),
      count_as_filed,
      open_count
    ))

  if (is.null(vars)) {
    data <- data |>
      dplyr::select(dplyr::all_of(columns), count_as_filed)
  } else {
    if (vars != "all") {
      selection <- c(dplyr::all_of(columns), dplyr::all_of(vars))
      data <- data |>
        dplyr::select(dplyr::all_of(selection))
    }
  }

  return(data)
}
