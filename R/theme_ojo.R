#' Style a ggplot in the OJO style
#'
#' Add OJO styling to a ggplot
#'
#' @examples
#' \dontrun{
#' theme_ojo()
#' }

theme_ojo <- function() {
  theme_bw(base_size=14, base_family="Menlo") %+replace%
    theme(
      panel.background  = element_blank(),
      plot.background = element_blank(),
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA)
    )
}

ojo_pal <- c("#F8D64E", "black", "#0D0887FF", "#6A00A8FF",
             "#B12A90FF", "#E16462FF", "#FCA636FF", "#F0F921FF")
