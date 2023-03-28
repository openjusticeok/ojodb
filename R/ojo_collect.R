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

  if(!.silent) {

    cli::cli_rule(left = "OJO Database Connection",
                  right = "{.emph ojodb {utils::packageVersion('ojodb')}}")

    cli::cli_progress_step("Searching ojodb for matching results...")

    t_1 <- Sys.time() # Timer start

    n_results <- query_tibble |>
      dplyr::tally() |>
      dplyr::pull(var = n) |>
      format(big.mark = ",")

    t_2 <- Sys.time() # Timer end

    cli::cli_progress_step(paste0("Found ", n_results, " matching results! Retrieving data now..."))

    if(difftime(t_2, t_1, units = "secs")) {
      cli::cli_alert_warning("If this step takes too long for your query, you can skip by setting `.silent = TRUE`")
    }

  }

  result <- query_tibble |>
    dplyr::collect()

  if(!.silent) { cli::cli_progress_step("Data retrieved!") }

  return(result)

}
