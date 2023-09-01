#' Query minutes for a given search term
#'
#' Query the Open Justice Oklahoma database for minutes matching a search term
#'
#' @param query A term or phrase to search for using the PostgreSQL full text search syntax
#' @param ... Placeholder
#' @param .con The OJO connection to use
#' @param .silent Should the command line interface elements be shown? Defaults to `TRUE` if the user is in an interactive session, and `FALSE` if they are not.
#'
#' @export ojo_search_minutes
#' @return data, a lazy tibble containing the resulting minutes
#' @examples
#' \dontrun{
#'
#' # Minutes containing words like 'mcgirt'
#' ojo_search_minutes("mcgirt")
#'
#' # Minutes containing 'mcgirt' AND 'jursdiction
#' ojo_search_minutes("mcgirt & jurisdiction")
#'
#' # Minutes containing 'mcgirt' OR 'jurisdiction
#' ojo_search_minutes("mcgirt | jurisdiction")
#'
#' # Minutes NOT containing 'mcgirt'
#' ## Use sparingly! It is likely to return rows in the hundreds of thousands or millions!
#' ojo_search_minutes("!! mcgirt")
#'
#' # Minutes containing 'tribal' FOLLOWED BY 'jurisdiction'
#' ojo_search_minutes("tribal <-> jurisdiction")
#' }
#'
ojo_search_minutes <- function(query, ..., .con = NULL, .silent = F) {

  if (is.null(.con)) {
    .con <- ojo_connect()
  }
  ojo_connect()

  if(!.silent){
    # CLI
    con_desc <- dbplyr::db_connection_describe(.con) |>
      gsub(pattern = "postgres", replacement = "") |>
      trimws()
    # Styling rules
    cli::cli_div(
      theme = list(
        rule = list(color = "br_yellow",
                    "line-type" = "single"),
        "span.grayed" = list(color = "grey")
      )
    )

    cli::cli_rule(left = paste("Connection:", con_desc),
                  right = "{.emph ojodb {utils::packageVersion('ojodb')}}")
    cli::cli_alert_info("Searching OJO database for matching minutes...")
  }

  # If query contains spaces and no postgres operators, wrap it in quotes
  query_clean <- dplyr::if_else(grepl(" ", query) & !grepl("&|\\||<->|\\!\\!", query),
                                paste0("'", query, "'"),
                                query)

  q <- glue::glue_sql(
    "SELECT * FROM minute WHERE to_tsvector('english', description) @@ to_tsquery('english', {query_clean});",
    .con = .con
  )

  df <- .con |>
    pool::dbGetQuery(q) |>
    dplyr::as_tibble()

  n_results <- nrow(df)

  if(!.silent){
    if (n_results > 0) {
      cli::cli_alert_success(paste0("Success! ",
                                    format(n_results, big.mark = ","),
                                    " matching results found."))
      return(df)
    } else {
      cli::cli_alert_warning("No matching results found.")
    }
  } else {
    return(df)
  }
}
