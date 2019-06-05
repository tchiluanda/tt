library(tidyverse)
library(viridis)
library(extrafont)

loadfonts()

tema <- function(){
  theme_minimal() +
    theme(
      text = element_text(family = "Open Sans", colour = "grey20"),
      title = element_text(face = "bold", size = 10, color = "#1E4C7A"), 
      plot.subtitle = element_text(family = "Open Sans Condensed", 
                                   color = "grey20", face = "plain", size = 10),
      axis.text = element_text(family = "Open Sans", colour = "grey20", size = 8),
      plot.caption = element_text(face = "italic"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      axis.ticks = element_line(size = 0.5),
      axis.ticks.length = unit(.25, "cm"),
      axis.title = element_text(size = 8, colour = "grey20"),
      legend.position = 'none')
}

