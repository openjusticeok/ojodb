#' Query civil cases from the OJO database
#'
#' Query the Open Justice Oklahoma database for civil cases with a case type of 'SC' (small claims)
#'
#' @param districts A character vector of districts to query
#' @param vars A character vector of variables to return
#' @param case_types A character vector of case types to query
#' @param file_years A character vector of years to query
#' @param ... Placeholder for additional arguments
#'
#' @export ojo_civ_cases ojo_add_issues
#' @return data, a lazy tibble containing the resulting civil cases
#'
#' @examples
#' \dontrun{
#' ojo_civ_cases()
#' ojo_civ_cases(districts = c("TULSA", "ADAIR"))
#' ojo_civ_cases(vars = "all")
#' ojo_civ_cases(vars = c("updated_at", "created_at"))
#' }
#'
ojo_civ_cases <- function(districts = "all", vars = NULL, case_types = c("CS", "SC", "CJ"),
                          file_years = lubridate::year(Sys.Date()), ...) {

  case_types_upper <- toupper(case_types)

  if (!all(case_types_upper %in% c("CS", "SC", "CJ"))) {
    stop("The 'case_types' argument must only include 'CS', 'SC', or 'CJ' cases.")
  }

  data <- ojo_tbl("case") |>
    dplyr::filter(
      .data$case_type %in% case_types_upper,
      .data$year %in% file_years
    )

  if (all(districts != "all")) {

    districts_upper <- toupper(districts)

    data <- data |>
      dplyr::filter(.data$district %in% districts_upper)
  }

  selection <- c("id", "district", "case_type", "date_filed", "date_closed")

  if (is.null(vars)) {
    data <- data |>
      dplyr::select(dplyr::all_of(selection)) |>
      ojo_add_issues()
    return(data)
  } else {
    if (any(vars == "all")) {
      data <- data |>
        ojo_add_issues()
      return(data)
    } else {
      selection <- append(selection, vars) |>
        unique()

      data <- data |>
        dplyr::select(dplyr::all_of(selection)) |>
        ojo_add_issues()

      return(data)
    }
  }
}

#' @title Add issues to civil cases
#'
#' @description Add issues to civil cases
#'
#' @param data A lazy tibble of civil cases
#' @param vars A character vector of variables to return
#' @param ... Placeholder for additional arguments
#'
#' @export ojo_add_issues
#' @return data, a lazy tibble containing the resulting civil cases

#' @examples
#' \dontrun{
#' ojo_civ_cases() |>
#'  ojo_add_issues()
#' }
#'
ojo_add_issues <- function(data, vars = NULL, ...) {
  if (!inherits(data, "tbl_lazy")) {
    stop("Don't use `collect()` before this function")
  }

  columns <- colnames(data)

  issues <- ojo_tbl("issue")

  if (is.null(vars)) {
    issues <- issues |>
      dplyr::select(
        case_id, rank, description, disposition, disposition_date
      )
  } else {
    if (vars != "all") {
      selection <- c("id", "disposition", "disposition_date", vars)
      issues <- issues |>
        dplyr::select(dplyr::all_of(selection))
    }
  }

  data <- data |>
    dplyr::left_join(issues,
      by = c("id" = "case_id"),
      suffix = c("", ".issue")
    )

  return(data)
}
