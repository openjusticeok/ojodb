#' Check whether the database connection is using SSL properly
#'
#' @export ojo_check_ssl
#'
#' @param ... Placeholder
#' @param .con The ojodb connection to use
#'
#' @return A logical indicator of whether the db connection is properly using SSL
#' @examples
#' \dontrun{
#' # Identifies the table
#' ojo_check_ssl()
#' }
#'
ojo_check_ssl <- function(..., .con = NULL) {
  if (is.null(.con)) {
    .con <- ojo_connect()
  }

  pool_src <- pool::poolCheckout(.con)
  on.exit(pool::poolReturn(pool_src))
  pool_src |>
    pool::dbGetQuery("select * from pg_stat_ssl where pid = pg_backend_pid();")
}
