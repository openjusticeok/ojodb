#' @title OJO Collect
#'
#' @description
#' A wrapper for dplyr::collect() that includes an ojodb progress spinner
#' Pull a lazy tibble into your local environment from the OJO database, using `ojo_crim_cases()`, `ojo_civ_cases()`, etc.
#'
#' @param .data The lazy tibble to be pulled
#' @param ... Placeholder for additional arguments
#' @param .silent Should the cli elements be shown? Defaults to `TRUE` if the user is in an interactive session, and `FALSE` if they are not.
#'
#' @export ojo_collect
#' @returns A local tibble
#'
#' @examples
#' \dontrun{
#' ojo_crim_cases() |>
#'  head(100) |>
#'  ojo_collect()
#' }
#'
ojo_collect <- function(.data, ..., .silent = !rlang::is_interactive()) {

  # Check class of `.data`
  if(!inherits(.data, "tbl_lazy")) {
    stop("`.data` must be a lazy tibble. Have you already collected the data?")
  }

  # Unlike in other functions, we don't need to check for a connection here, because
  # the connection is checked in the ojo_*() functions that return a lazy tibble.
  # That means that `.data` should always have a connection attached to it.
  # If it doesn't, then the user is doing something wrong, and we should let them know.

  # First check that `.data` is a lazy tibble created with `ojo_connect()`, `pool::dbPool()`, or `DBI::dbConnect()`
  if(!inherits(.data, "tbl_Pool") && !inherits(.data, "tbl_dbi")) {
    stop("`.data` must be a lazy tibble created with `ojo_connect`, `pool::dbPool`, `DBI::dbConnect`. Have you already collected the data?")
  }

  # Extract the connection from the lazy tibble
  .con <- dbplyr::remote_con(.data)

  # If the user is using `pool`, we need to check out a connection from the pool locally
  # `pool::localCheckout()` will return the connection to the pool when the function exits
  if(inherits(.data, "tbl_Pool")) {
    .con <- .con |> pool::localCheckout()
  }

  # Check connection validity
  if(!DBI::dbIsValid(.con)) {
    stop("The connection to the OJO database is no longer valid. Please reconnect to the database using `ojo_connect()`.")
  }

  # First UI chunk
  if(!.silent) {
    con_desc <- dbplyr::db_connection_describe(.con) |>
      gsub(pattern = "postgres", replacement = "") |>
      trimws()

    # cli styling rules
    cli::cli_div(
      theme = list(
        rule = list(color = "br_yellow",
                    "line-type" = "single"),
        "span.grayed" = list(color = "grey")
      )
    )

    cli::cli_rule(left = paste("Connection:", con_desc),
                  right = "{.emph ojodb {utils::packageVersion('ojodb')}}")

    cli::cli_progress_step(msg = "Searching OJO database for matching results...",
                           msg_failed = "Something went wrong sending your query to the database! Please check your connection.")
  }

  ## TODO: Use proper sql render method from collect method on tbl_sql
  # Translate query back to SQL
  query <- dplyr::show_query(.data) |>
    capture.output() |>
    paste(collapse = "\n") |>
    gsub(pattern = "^<SQL>\n", replacement = "")

  # Make initial db request
  request <- DBI::dbSendQuery(.con, query)

  # Get n rows in request results
  t_0 <- Sys.time() # Timer start

  ## TODO: This needs work; what happens if the .data is grouped? Has a column named `n`?
  n_results <- .data |>
    dplyr::ungroup() |>
    dplyr::tally() |>
    dplyr::pull(var = .data$n)

  t_1 <- Sys.time() # Timer end

  # Second UI chunk
  if(!.silent) {
    cli::cli_progress_step(msg = paste0("Found ", format(n_results, big.mark = ","), " matching results!"),
                           msg_failed = "Something went wrong downloading your data from the database!")

    # Warning if it took more than 20 seconds
    if(difftime(t_1, t_0, units = "secs") > 20) {
      cli::cli_alert_warning("If the previous step took too long for your query, you can skip these progress updates by setting `.silent = TRUE`")
    }
  }

  # Downloading from request
  res <- NULL

  chunk_size <- round(n_results / 100, 0) # This way we're downloading in chunks of 1% of the total or 1000, whichever is bigger;
  if(chunk_size < 1000) { chunk_size <- 1000 } # might help with large queries

  cli::cli_progress_bar(
    name = "dl_pb",
    format = "\u001b[34m{cli::symbol$info}\u001b[0m Downloading data... {cli::pb_bar} {cli::pb_percent} | ~{cli::pb_eta} remaining...",
    format_done = "\u001b[32m{cli::symbol$tick}\u001b[0m Data retrieved successfully! {cli::pb_bar} {cli::pb_percent} | {.grayed [{cli::pb_elapsed}]}",
    type = "iterator",
    total = n_results,
    clear = FALSE
  )

  while (!DBI::dbHasCompleted(request)) {
    # Fetch the next chunk of data
    chunk <- DBI::dbFetch(request, n = chunk_size)
    if (nrow(chunk) == 0) {
      # No more data to fetch
      break
    }

    # Combine the data with previous chunks
    res <- rbind(res, tibble::as_tibble(chunk))

    # update pb
    cli::cli_progress_update(set = nrow(res))
  }

  # Terminate pb
  cli::cli_progress_done()

  # Clear the result set after final dbFetch call
  DBI::dbClearResult(request)

  return(res)
}
