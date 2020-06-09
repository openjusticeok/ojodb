#' Close all connections to the \code{ojo} database
#'
#' @examples
#' ojo_disconnect_all()


ojo_disconnect_all <- function() {
  lapply(dbListConnections(dbDriver(drv = "MySQL")), dbDisconnect)
}

