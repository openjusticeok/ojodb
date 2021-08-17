#' Query civil cases from ojodb
#'
#' Query the Open Justice Oklahoma database for civil cases with a casetype of'SC' (small claims)
#'
#' @export ojo_civ_cases ojo_add_issues
#' @return data, a lazy tibble containing the resulting civil cases
#' @examples
#' \dontrun{
#' ojo_civ_cases()
#' ojo_civ_cases(districts = c("TULSA", "ADAIR"))
#' ojo_civ_cases(vars = "all")
#' ojo_civ_cases(vars = c("updated_at", "created_at"))
#' }
#'

ojo_civ_cases <- function(districts = "all", vars = NULL, case_types = c("CS", "SC", "CJ"),
                          file_years = 2000:year(Sys.Date()), ...) {
  data <- ojo_tbl("case") |>
    filter(case_type %in% case_types,
           year %in% file_years)

  if(districts != "all") {
    data <- data |>
      filter(district %in% districts)
  }

  selection <- c("id", "district", "case_type", "date_filed", "date_closed")

  if(is.null(vars)) {
    data <- data |>
      select(all_of(selection))
    return(data)
  } else {
    if(vars == "all") {
      return(data)
    } else {
      selection <- append(selection, vars) |>
        unique()

      data <- data |>
        select(all_of(selection))

      return(data)
    }
  }
}

ojo_add_issues <- function(data, vars = NULL, ...) {
  if(!"tbl_lazy" %in% class(data)) {
    stop("Don't use `collect()` before this function")
  }

  columns <- colnames(d)

  counts <- ojo_tbl("issue")

  if(is.null(vars)) {
    counts <- counts |>
      select(case_id, rank, description,
             disposition, disposition_date)
  } else {
    if(vars != "all") {
      selection <- c(id, disposition, disposition_date, vars)
      counts <- counts |>
        select(all_of(selection))
    }
  }

  data <- data |>
    left_join(counts,
              by = c("id" = "case_id"),
              suffix = c("", ".issue"))

  return(data)
}

