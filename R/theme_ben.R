#' theme_ben
#'
#' @param base_size base font size, given in pts
#'
#' @return Creates a theme which controls all non-data display.
#' @export
#'
#' @examples
theme_ben <- function(base_size = 14) {
    theme_classic(base_size = base_size) %+replace%
        theme(
            # L'ensemble de la figure  set the format of figure title and subtitle
            plot.title = element_text(size = rel(0.8), face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size=rel(0.7),hjust = 0.5),
            # Zone où se situe le graphique  set grid and border of the figure invisible
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            # Les axes set the format of the axis
            axis.title = element_text(size = rel(0.6),face="bold"),
            axis.text = element_text(size = rel(0.6)),
            axis.line = element_line(color = "black"),
            # La légende set the format of the legend
            legend.title = element_text(size = rel(0.1), face = "bold"),
            legend.text = element_text(size = rel(0.50), face = "bold"),
            legend.key = element_rect(fill = "white", colour = "white"),
            legend.key.size = unit(0.5, "lines"),
            legend.background = element_rect(fill = "white", colour = "white"),
            legend.position="bottom",
            # Les étiquettes dans le cas d'un facetting set the format of the strip
            strip.background = element_rect(fill = "#17252D", color = "#17252D"),
            strip.text = element_text(size = rel(0.5),
                                      face = "bold",
                                      color = "white",
            )
        )
}

