#' Add counts to criminal case query
#'
#' Query the Open Justice Oklahoma database for criminal cases with a casetype of'CM' (misdemeanor) or 'CF' (felony)
#'
#' @export ojo_add_counts
#' @return data, a lazy tibble containing the resulting criminal cases
#' @examples
#' \dontrun{
#' ojo_crim_cases()
#' ojo_crim_cases(districts = c("TULSA", "ADAIR"))
#' ojo_crim_cases(vars = "all")
#' ojo_crim_cases(vars = c("updated_at", "created_at"))
#' }
#'

ojo_add_counts <- function(data, vars = NULL, ...) {
  if(!"tbl_lazy" %in% class(data)) {
    stop("Don't use `collect()` before this function")
  }

  columns <- colnames(data)

  counts <- ojo_tbl("count")

  if(is.null(vars)) {
    counts <- counts |>
      select(case_id, rank, party, count_as_filed, violation_of, date_of_offense,
             count_as_disposed, disposition, disposition_detail, disposition_date)
  } else {
    if(vars != "all") {
      selection <- c(case_id, disposition, disposition_date, vars)
      counts <- counts |>
        select(all_of(selection))
    }
  }

  open_counts <- ojo_tbl("case") %>%
    filter(district == "ALFALFA",
           case_type == "CF") %>%
    mutate(open_count = sql("UNNEST(open_counts)"),
           party = sql("UNNEST(parties)")) %>%
    left_join(ojo_tbl("party"),
              by = c("party" = "id")) %>%
    filter(role == "Defendant") %>%
    select(id, open_count, name, odcr_id = oscn_id)

  data <- data |>
    left_join(counts,
              by = c("id" = "case_id"),
              suffix = c("", ".count")) %>%
    left_join(open_counts,
              by = c("id" = "id")) %>%
    left_join(ojo_tbl("party") %>%
                select(id, name, oscn_id),
              by = c("party" = "id")) %>%
    group_by(id) %>%
    mutate(count_as_filed = if_else(is.na(count_as_filed),
                                    open_count,
                                    count_as_filed),
           rank = if_else(is.na(rank),
                          row_number() - 1,
                          rank),
           defname = if_else(is.na(name.x),
                             name.y,
                             name.x),
           oscn_id = if_else(is.na(oscn_id),
                             odcr_id,
                             oscn_id)
    ) %>%
    ungroup %>%
    select(district, case_number, case_type, date_filed,
           date_closed,
           rank, defname, oscn_id, count_as_filed, violation_of,
           date_of_offense, count_as_disposed,
           disposition, disposition_detail, disposition_date)

  return(data)
}

