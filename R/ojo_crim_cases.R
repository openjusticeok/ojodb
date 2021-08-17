#' Query criminal cases from ojodb
#'
#' Query the Open Justice Oklahoma database for criminal cases with a casetype of'CM' (misdemeanor) or 'CF' (felony)
#'
#' @export ojo_crim_cases ojo_add_counts
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
                           file_years = 2000:year(Sys.Date()), ...) {
  data <- ojo_tbl("case") |>
    filter(case_type %in% case_types,
           year %in% file_years)

  if(districts != "all") {
    data <- data |>
      filter(district %in% districts)
  }

  selection <- c("id", "district", "case_number", "case_type", "date_filed", "date_closed")

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


ojo_add_counts <- function(data, vars = NULL, ...) {
  if(!"tbl_lazy" %in% class(data)) {
    stop("Don't use `collect()` before this function")
  }

  columns <- colnames(data)

  counts <- ojo_tbl("count")

  if(is.null(vars)) {
    counts <- counts |>
      select(case_id, rank, count_as_filed, violation_of, date_of_offense,
             count_as_disposed, disposition, disposition_detail, disposition_date)
  } else {
    if(vars != "all") {
      selection <- c(case_id, disposition, disposition_date, vars)
      counts <- counts |>
        select(all_of(selection))
    }
  }

  data <- data |>
    left_join(counts,
              by = c("id" = "case_id"),
              suffix = c("", ".count"))

  return(data)
}

