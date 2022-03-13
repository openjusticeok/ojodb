#' Check whether the database connection is using SSL properly
#'
#' @export ojo_check_ssl
#'
#' @return A logical indicator of whether the db connection is properly using SSL
#' @examples
#' \dontrun{
#' # Identifies the table
#' ojo_check_ssl()
#'}
#'
ojo_check_ssl <- function() {

  if(!exists("ojodb", where = .GlobalEnv)) {
    ojo_connect()
  }

  pool_src <- poolCheckout(ojodb)
  on.exit(poolReturn(pool_src))
  pool_src |>
    dbGetQuery("select * from pg_stat_ssl where pid = pg_backend_pid();")
}
