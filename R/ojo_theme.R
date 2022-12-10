#' Style a ggplot in the OJO style
#'
#' Add OJO styling to a ggplot
#'
#' @export ojo_theme
#'
#' @importFrom ggplot2 %+replace%
#'
#' @examples
#' \dontrun{
#' ggplot(ojo_example, aes(file_year, n_cases, color = court)) +
#'   geom_line(size = 1.5) +
#'   ojo_theme() +
#'   ojo_colors() +
#'   scale_x_continuous(
#'     breaks = 2010:2019,
#'     limits = c(NA, 2019)
#'   )
#' }
ojo_theme <- function() {
  ggplot2::theme_bw(base_size = 14, base_family = "Menlo") %+replace%
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.key = ggplot2::element_rect(fill = "transparent", colour = NA)
    )
}
