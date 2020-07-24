#' Give a plot
#'
#' Add OJO styling to a ggplot
#'
#' @examples
#' \dontrun{
#' ggplot(ojo_example, aes(file_year, n_cases, color = court)) +
#'    geom_line(size = 1.5) +
#'    theme_ojo() +
#'    ojo_colors() +
#'    scale_x_continuous(breaks = 2010:2019,
#'                    limits = c(NA, 2019))
#' }

ojo_colors <- function(numbers = 1:8) {
  scale_color_manual(values = ojo_pal[numbers]) +
    scale_fill_manual(values = ojo_pal[numbers])
}

