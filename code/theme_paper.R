
#' Declare and set ggplot theme.
#'
#' @param
#'
#' @return 
#'
#' @export

theme_paper <- function(base_size = 11, base_family = "Avenir Book") {
  return <- theme_minimal(base_size, base_family) +
    theme(plot.title = element_text(size = rel(1.4), face = "plain",
                                    family = "Avenir Medium"),
          plot.subtitle = element_text(size = rel(1), face = "plain",
                                       family = "Avenir Book"),
          plot.caption = element_text(size = rel(0.8), color = "#7f7f7f", face = "plain",
                                      family = "Avenir Book",
                                      margin = margin(t = 10)),
          plot.tag = element_text(size = rel(1), face = "plain", color = "#7f7f7f",
                                  family = "Avenir Book"),
          strip.text = element_text(size = rel(0.8), face = "plain",
                                    family = "Avenir Book"),
          strip.text.x = element_text(margin = margin(t = 1, b = 1)),
          panel.border = element_blank(),
          plot.background = element_rect(fill="#ffffff", colour = NA),
          axis.ticks = element_blank(),
          panel.grid = element_line(colour="#ffffff"),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)),
          legend.title = element_text(size = rel(0.8)),
          legend.position = "bottom")
  return
}
theme_set(theme_paper())
