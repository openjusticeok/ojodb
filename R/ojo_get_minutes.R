

ojo_get_minutes <- function(data) {
  vars <- colnames(data)

  minutes <- ojo_tbl("minute")

  data <- data |>
    left_join(minutes,
              by = c("id" = "case_id"),
              suffix = c("", ".minute"))

  return(data)
}


