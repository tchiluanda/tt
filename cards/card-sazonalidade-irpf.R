library(tidyverse)
library(viridis)
library(extrafont)
library(readxl)

loadfonts()

tema <- function(){
  theme_minimal() +
    theme(
      text = element_text(family = "Montserrat", colour = "grey20"),
      title = element_text(face = "bold", size = 10, color = "#1E4C7A"), 
      plot.subtitle = element_text(family = "Montserrat Thin", 
                                   color = "grey20", face = "plain", size = 10),
      axis.text = element_text(family = "Montserrat", colour = "grey20", size = 8),
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

meses <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
MESES <- str_to_upper(meses)

nomes_meses <- meses
names(nomes_meses) <- 1:12

dados_originais <- read.csv2("rec_irpf_2019_04.csv")

dados <- dados_originais %>%
  select(-2) %>%
  mutate(Data = lubridate::dmy(Data),
         ano = lubridate::year(Data),
         mes = lubridate::month(Data),
         nome_mes = factor(meses[mes], levels = meses))

ggplot(dados, aes(y = valor, x = nome_mes, color = ano, group = ano)) +
  geom_line() +
  coord_polar() +
  scale_color_viridis(option = "magma", direction = -1) + 
  tema()

scale_x_discrete(labels = nomes_meses) +
