#' Close all connections to the \code{ojo} database
#'
#' @examples
#' \dontrun{
#' ojo_disconnect_all()
#' }


ojo_disconnect_all <- function() {
  lapply(dbListConnections(dbDriver(drv = "MySQL")), dbDisconnect)
}

