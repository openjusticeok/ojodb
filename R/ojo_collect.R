#' @title OJO Collect
#'
#' @description
#' A wrapper for dplyr::collect() that includes a progress bar
#' Pull a lazy tibble into your local environment from the OJO database, using `ojo_crim_cases()`, `ojo_civ_cases()`, etc.
#'
#' @param .data The lazy tibble to be pulled
#' @param ... Placeholder for additional arguments
#' @param .silent Should the command line interface elements be shown? Defaults to `TRUE` if the user is in an interactive session, and `FALSE` if they are not.
#'
#' @export ojo_collect
#'
#' @import dplyr
#'
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
  if (!inherits(.data, c("tbl_lazy", "tbl_Pool", "tbl_dbi"))) {
    rlang::abort("`.data` must be a lazy tibble created with `ojo_connect()`, `pool::dbPool()`, or `DBI::dbConnect()`.")
  }

  # Extract the database connection
  .con <- dbplyr::remote_con(.data)
  if (inherits(.data, "tbl_Pool")) {
    .con <- pool::localCheckout(.con)
  }

  # Ensure the connection is valid
  if (!DBI::dbIsValid(.con)) {
    rlang::abort("The connection to the OJO database is no longer valid. Please reconnect to the database using `ojo_connect()`.")
  }

  # CLI output
  if (!.silent) {
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

  # Estimate number of results and warn about potential issues
  n_results <- estimate_results(.data, .silent)

  # Collect data
  res <- collect_data(.con, .data, n_results, .silent)

  return(res)
}


estimate_results <- function(.data, .silent) {
  if ("n" %in% names(.data)) {
    rlang::warn("The tbl you are requesting has a variable named `n`. This might cause issues with progress bar rendering.",
                .frequency = "once", .frequency_id = "ojo_collect_n_warning")
  }

  n_results <- .data |>
    dplyr::ungroup() |>
    dplyr::tally() |>
    dplyr::pull(n = n)

  if (!.silent) {
    cli::cli_progress_step(msg = paste0("Found ", format(n_results, big.mark = ","), " matching results!"))
  }

  return(n_results)
}

collect_data <- function(.con, .data, n_results, .silent) {
  query <- dbplyr::sql_render(.data)
  req <- DBI::dbSendQuery(.con, query)
  withr::defer(DBI::dbClearResult(req), envir = parent.frame())

  res <- NULL
  chunk_size <- max(round(n_results / 100, 0), 1000) # Download in chunks of 1% or at least 1000

  if (!.silent) {
    init_progress_bar(n_results)
  }

  while (!DBI::dbHasCompleted(req)) {
    chunk <- DBI::dbFetch(req, n = chunk_size)
    res <- rbind(res, tibble::as_tibble(chunk))

    if (!.silent) {
      cli::cli_progress_update(set = nrow(res))
    }
  }

  if (!.silent) {
    cli::cli_progress_done()
  }

  return(res)
}

init_progress_bar <- function(n_results) {
  cli::cli_progress_bar(name = "dl_pb", total = n_results, clear = FALSE)
}
