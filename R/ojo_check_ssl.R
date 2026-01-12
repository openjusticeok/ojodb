#' Check whether the database connection is using SSL properly
#'
#' @export ojo_check_ssl
#'
#' @param con The ojodb connection to use
#'
#' @return A logical indicator of whether the db connection is properly using SSL
#' @examples
#' \dontrun{
#' # Check SSL status for the default connection
#' ojo_check_ssl()
#' }
#'
ojo_check_ssl <- function(con = NULL) {
  if (is.null(con)) {
    con <- ojo_default_connection()
  }

  if (inherits(con, "Pool")) {
    pool_src <- pool::poolCheckout(con)
    on.exit(pool::poolReturn(pool_src), add = TRUE)
  } else {
    pool_src <- con
  }

  pool_src |>
    DBI::dbGetQuery("select * from pg_stat_ssl where pid = pg_backend_pid();")
}
