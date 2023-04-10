#' A wrapper for dplyr::collect() that includes an ojodb progress spinner
#'
#' Pull a lazy tibble into your local environment from the OJO database, using `ojo_crim_cases()`, `ojo_civ_cases()`, etc.
#'
#' @param query_tibble The lazy tibble to be pulled; generated using `ojo_crim_cases()`, `ojo_civ_cases()`, etc.
#' @param .silent Should the cli elements be shown?
#' @param ... Placeholder for additional arguments
#'
#' @export ojo_collect
#' @return data, a local tibble containing the resulting criminal cases
#' @examples
#' \dontrun{
#' ojo_crim_cases() |> ojo_collect()
#' }
#'
ojo_collect <- function(query_tibble, .silent = FALSE, ...) {

  .con <- ojo_connect()

  # First UI chunk
  if(!.silent) {

    con_desc <- dbplyr::db_connection_describe(.con) |>
      gsub(pattern = "postgres", replacement = "") |>
      trimws()

    div_cli <- cli::cli_div(theme = list(rule = list(
      color = "br_yellow",
      "line-type" = "single"
    )))

    cli::cli_rule(left = paste("Connection:", con_desc),
                  right = "{.emph ojodb {utils::packageVersion('ojodb')}}")

    cli::cli_progress_step("Searching OJO database for matching results...")

  }

  # Translate query back to SQL
  query <- dplyr::show_query(query_tibble) |>
    capture.output() |>
    paste(collapse = "\n") |>
    gsub(pattern = "^<SQL>\n", replacement = "")

  # Make initial db request
  request <- DBI::dbSendQuery(pool::localCheckout(.con), query)

  # Get n rows in request results
  t_1 <- Sys.time() # Timer start
  n_results <- query_tibble |>
    dplyr::tally() |>
    dplyr::pull(var = n)

  # Can't get this to work for some reason, should be faster than the above
  # DBI::dbGetRowCount(request)

  t_2 <- Sys.time() # Timer end

  # Second UI chunk
  if(!.silent){
    cli::cli_progress_step(paste0("Found ", format(n_results, big.mark = ","), " matching results! Retrieving data now..."))

    # Warning if it took more than 20 seconds
    if(difftime(t_2, t_1, units = "secs") > 20) {
      cli::cli_alert_warning("If the previous step took too long for your query, you can skip these progress updates by setting `.silent = TRUE`")
    }
  }

  # Downloading from request
  result <- NULL
  chunk_size <- 1000

  cli::cli_progress_bar("Downloading data...",
                        type = "iterator",
                        total = n_results)

  while (!DBI::dbHasCompleted(request)) {
    # Fetch the next chunk of data
    chunk <- DBI::dbFetch(request, n = chunk_size)
    if (nrow(chunk) == 0) {
      # No more data to fetch
      break
    }

    # Combine the data with previous chunks
    if (is.null(result)) {
      result <- chunk
    } else {
      result <- rbind(result, chunk)
    }

    # update pb
    cli::cli_progress_update(set = nrow(result))
  }

  # Old method
  # result <- query_tibble |>
  #   dplyr::collect()

  if(!.silent) { cli::cli_progress_step("Data retrieved!") }

  return(result)

}

