#' Add a reform event line to a plot
#'
#' Adds a labeled vertical line to a line graph. Especially useful for marking where a reform event occurred in impact evaluations.
#'
#' @examples
#' \dontrun{
#' ggplot(ojo_example, aes(file_year, n_cases, color = court)) +
#'    geom_line(size = 1.5) +
#'    theme_ojo() +
#'    ojo_colors() +
#'    scale_x_continuous(breaks = 2010:2019,
#'                    limits = c(NA, 2019)) +
#'    ojo_reform_line("2018-11-01", "2018 reforms", 500)
#' }

ojo_reform_line <- function(ymd, label, y) {
  list(geom_vline(aes(xintercept = ymd(ymd)),
                  linetype = "dashed"),
       annotate("label", ymd(ymd), y,
                label = label,
                family = "Menlo")
  )
}
