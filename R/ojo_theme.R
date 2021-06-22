#' Style a ggplot in the OJO style
#'
#' Add OJO styling to a ggplot
#'
#' @examples
#' \dontrun{
#' ggplot(ojo_example, aes(file_year, n_cases, color = court)) +
#'    geom_line(size = 1.5) +
#'    ojo_theme() +
#'    ojo_colors() +
#'    scale_x_continuous(breaks = 2010:2019,
#'                    limits = c(NA, 2019))
#' }

ojo_theme <- function() {
  theme_bw(base_size=14, base_family="Menlo") %+replace%
    theme(
      panel.background  = element_blank(),
      plot.background = element_blank(),
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA)
    )
}
