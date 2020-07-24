date_to_fy <- function(date) {
  ifelse(month(date) > 6, year(date) + 1, year(date))
}
