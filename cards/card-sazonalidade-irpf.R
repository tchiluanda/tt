library(tidyverse)
library(viridis)
library(extrafont)
library(readxl)
library(gganimate)
library(transformr)

loadfonts()

tema <- function(){
  theme_minimal() +
    theme(
      text = element_text(family = "Montserrat", colour = "grey20"),
      title = element_text(face = "plain", size = 10, color = "#280B54"), 
      plot.subtitle = element_text(family = "Montserrat Light", 
                                   color = "grey20", face = "plain", size = 10),
      axis.text = element_text(family = "Montserrat", colour = "grey20", size = 8),
      plot.caption = element_text(face = "italic", hjust = 0),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      axis.ticks = element_line(size = 0.5),
      axis.ticks.length = unit(.25, "cm"),
      axis.title = element_text(size = 8, colour = "grey20"),
      legend.position = 'none')
}

complemento_tema_gif <- function(){
  theme(
    title = element_text(size = 14), 
    plot.subtitle = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )
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

# pense na ginástica para inverter a ordem das áreas... para inverter precisava transformar em fator, mas em fator não conseguiria usar a escala contínua do viridis, aí reconverti para numérico e deu certo.

graf_area <- function(opcao_paleta) { ggplot(dados %>% mutate(
  ano_inv = factor(ano, levels = rev(unique(dados$ano)))),
                    aes(x = nome_mes, group = ano_inv, 
                        fill = as.numeric(as.character(ano_inv)))) +
  geom_area(aes(y = ifelse(ano == max(dados$ano), valor, NA)), 
                color = NA, stat = "identity", position = 'identity') +
  geom_polygon(aes(y = ifelse(ano != max(dados$ano), valor, NA)), 
            color = NA, stat = "identity", position = 'identity') +
  coord_polar(start = -pi/12) +
  scale_fill_viridis(option = opcao_paleta, 
                     direction = -1, 
                     breaks = c(max(dados$ano), min(dados$ano)),
                     guide = guide_colourbar(ticks = FALSE)) +
  scale_y_continuous(labels = function(v){format(v/1e6, big.mark = ".", decimal.mark = ",")})+
  labs(x = NULL, y = "R$ milhões", fill = NULL,
       title = "Sazonalidade da arrecadação mensal do IRPF",
       subtitle = "Valores de jan/1997 a abr/2019, atualizados pelo IPCA",
       caption = "Fonte: \"Ressignificando o Resultado do Tesouro Nacional\"") +
  tema() + theme(panel.grid.major.x = element_line(size = 0.25,  color = "#555555", #para sair no png. na visualização aqui usei size = .05 e color = #DDDDDD
                                                   linetype = "dotted"),
                 legend.text = element_text(size = 8),
                 legend.position = "right")
}

paletas <- c("viridis", "magma", "plasma", "inferno")

graficos_areas <- purrr::map(paletas, graf_area)
names(graficos_areas) <- paletas

ggsave("graf_area_gg.png", plot = graficos_areas[["inferno"]],
       width=6, height=6, dpi = 500, pointsize = 10) #9.36 x 6


graficos_areas[["magma"]] +  
  transition_reveal(ano) +
  shadow_mark(alpha = .5)

# 
# graf_estatico <- ggplot(dados, aes(x = nome_mes, group = ano)) +
#   geom_polygon(aes(color = ano, y = ifelse(ano != max(dados$ano), valor, NA)), fill = NA) + # geom_polygon fecha a figura do ano
#   geom_line(aes(color = ano, y = ifelse(ano == max(dados$ano), valor, NA))) +
#   coord_polar(start = -pi/12) +
#   scale_color_viridis(option = "magma", 
#                      direction = -1, 
#                      breaks = c(max(dados$ano), min(dados$ano)),
#                      guide = guide_colourbar(ticks = FALSE)) +
#   # scale_fill_viridis(option = "magma", 
#   #                    direction = 1, 
#   #                    breaks = c(max(dados$ano), min(dados$ano))) +
#   scale_y_continuous(labels = function(v){format(v/1e6, big.mark = ".", decimal.mark = ",")})+
#   labs(x = NULL, y = "R$ milhões", fill = NULL, color = NULL) +
#   tema() + theme(panel.grid.major.x = element_line(size = 0.15,  color = "grey50",
#                                                    linetype = "dotted"),
#                  legend.text = element_text(size = 8),
#                  legend.position = "right",
#                  axis.title.y = element_text(hjust = 0.6)) #angle = 0
# 
# ggsave("graf.png", width=9.36, 
#        height=6, dpi = 500)
# 
# png(filename="graf_cairo.png", 
#     type="cairo",
#     units="in", 
#     width=9.36, 
#     height=6, 
#     pointsize=12, 
#     res=500)
# graf_estatico
# dev.off()

cor <- "#280B54"

graf <- ggplot(dados, aes(x = nome_mes, group = ano)) +
  geom_line(aes(y = valor),
            color = cor,
            size = 1.5) +
  coord_polar(start = -pi/12) +
  scale_y_continuous(labels = function(v){format(v/1e6, big.mark = ".", decimal.mark = ",")})+
  labs(x = NULL, y = "R$ milhões", fill = NULL, color = NULL,
       title = "Sazonalidade da arrecadação mensal do IRPF",
       subtitle = "Valores de jan/1997 a abr/2019, atualizados pelo IPCA",
       caption = "Fonte: \"Ressignificando o Resultado do Tesouro Nacional\"") +
  tema() + theme(panel.grid.major.x = element_line(size = 0.25,  color = "#666666",
                                                   linetype = "dotted"),
                 legend.text = element_text(size = 8),
                 axis.title.y = element_text(hjust = 0.6)) #angle = 0

graf2 <- ggplot(dados, aes(x = nome_mes, group = ano)) +
  geom_line(aes(y = valor, color = ano),
            size = 1.5) +
  coord_polar(start = -pi/12) +
  scale_y_continuous(labels = function(v){format(v/1e6, big.mark = ".", decimal.mark = ",")})+
  scale_color_viridis(option = "magma", 
                      direction = -1, 
                      breaks = c(max(dados$ano), min(dados$ano))) +
  labs(x = NULL, y = "R$ milhões", fill = NULL, color = NULL,
       title = "Sazonalidade da arrecadação mensal do IRPF",
       subtitle = "Valores de jan/1997 a abr/2019, atualizados pelo IPCA",
       caption = "Fonte: \"Ressignificando o Resultado do Tesouro Nacional\"") +
  tema() + theme(panel.grid.major.x = element_line(size = .4,  color = "#444444",
                                                   linetype = "dotted"),
                 legend.text = element_text(size = 8),
                 axis.title.y = element_text(hjust = 0.6)) #angle = 0

graf_anim <- graf + theme(legend.position = "none") + labs(title = '{closest_state}') +
  transition_states(ano,
                    transition_length = 2,
                    state_length = 1) +
  #ease_aes('quadratic-in-out') +
  shadow_mark(alpha = .5, size = 1, color = "grey40")

graf_anim2 <- graf2 + complemento_tema_gif() + 
  labs(subtitle = 'Valores de {closest_state}, atualizados pelo IPCA') +
  transition_states(ano,
                    transition_length = 2,
                    state_length = 1) +
  shadow_mark(alpha = .2, size = 1)

graf_anim %>% animate(type = "cairo", nframes = 150)
graf_anim2 %>% animate(type = "cairo", nframes = 210)

anim_save("sazonalidade_4.gif", animation = last_animation())



# graf barra --------------------------------------------------------------

sazo_barra <- ggplot(dados %>% arrange(desc(ano)),
                    aes(x = nome_mes,
                        fill = cor)) + #ano)) +
  geom_col(aes(y = valor), 
           alpha = 1,
           width = 1,
           color = NA, position = 'identity') +
  coord_polar(start = -pi/12) +
  scale_fill_viridis(option = "magma", 
                     direction = -1, 
                     breaks = c(max(dados$ano), min(dados$ano)),
                     guide = guide_colourbar(ticks = FALSE)) +
  scale_y_continuous(labels = function(v){format(v/1e6, big.mark = ".", decimal.mark = ",")})+
  labs(x = NULL, y = "R$ milhões", fill = NULL,
       title = "Sazonalidade da arrecadação mensal do IRPF",
       subtitle = "Valores de jan/1997 a abr/2019, atualizados pelo IPCA",
       caption = "Fonte: \"Ressignificando o Resultado do Tesouro Nacional\"") +
  tema() + theme(panel.grid.major.x = element_line(size = 0.25,  color = "#555555", #para sair no png. na visualização aqui usei size = .05 e color = #DDDDDD
                                                   linetype = "dotted"),
                 legend.text = element_text(size = 8),
                 legend.position = "right")

ggsave("graf_barra.png", plot = sazo_barra,
       width=6, height=6, dpi = 500, pointsize = 10) #9.36 x 6

sazo_barra_anim1 <- sazo_barra +
  labs(subtitle = 'Valores de {closest_state}, atualizados pelo IPCA') +
  transition_reveal(ano)

sazo_barra_anim2 <- sazo_barra +
  labs(subtitle = 'Valores de {closest_state}, atualizados pelo IPCA') +
  transition_states(ano,
                    transition_length = 2,
                    state_length = 1) +
  shadow_mark(alpha = .5, size = 1, fill = "grey40")

sazo_barra_anim2 %>% animate(type = "cairo", nframes = 150)

anim_save("sazonalidade_barra.gif", animation = last_animation())
