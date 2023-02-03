#' Add OJO styling to a ggplot
#'
#' Add OJO styling to a ggplot
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(ojo_example, aes(file_year, n_cases, color = court)) +
#'   geom_line(size = 1.5) +
#'   theme_ojo() +
#'   ojo_color() +
#'   scale_x_continuous(
#'     breaks = 2010:2019,
#'     limits = c(NA, 2019)
#'   )
#' }
#'
ojo_pal <- c(
  "#F8D64E", "black", "#0D0887FF", "#6A00A8FF",
  "#B12A90FF", "#E16462FF", "#FCA636FF", "#F0F921FF"
)


#' Add OJO styling to a ggplot
#'
#' Add OJO styling to a ggplot
#'
#' @param numbers A vector of numbers to use for the color palette
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(ojo_example, aes(file_year, n_cases, color = court)) +
#'   geom_line(size = 1.5) +
#'   theme_ojo() +
#'   ojo_color() +
#'   scale_x_continuous(
#'     breaks = 2010:2019,
#'     limits = c(NA, 2019)
#'   )
#' }
#'
ojo_color <- function(numbers = 1:8) {
  ggplot2::scale_color_manual(values = ojo_pal[numbers])
}


#' Add OJO styling to a ggplot
#'
#' Add OJO styling to a ggplot
#'
#' @param numbers A vector of numbers to use for the color palette
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(ojo_example, aes(file_year, n_cases, color = court)) +
#'   geom_line(size = 1.5) +
#'   theme_ojo() +
#'   ojo_color() +
#'   scale_x_continuous(
#'     breaks = 2010:2019,
#'     limits = c(NA, 2019)
#'   )
#' }
#'
ojo_fill <- function(numbers = 1:8) {
  ggplot2::scale_fill_manual(values = ojo_pal[numbers])
}
