library(tidyverse)
library(extrafont)
library(ggtext)

loadfonts()

# construindo uma base para a visualizacao --------------------------------

qde_fundos_visados      <- 163
qde_fundos_com_fin      <- 54
qde_fundos_sem_fin      <- 106
qde_fundos_com_bp_nofss <- 3
qde_fundos_com_sf       <- 47
qde_fundos_com_bp       <- 44
qde_fundos_com_sfbp     <- 37

qde_fundos_so_sf        <- qde_fundos_com_sf - qde_fundos_com_sfbp
qde_fundos_so_bp        <- qde_fundos_com_bp - qde_fundos_com_sfbp

lista_fundos <- c("Com superávit financeiro apenas"      = qde_fundos_so_sf, 
                  "Com superávit financeiro e balanço patrimonial"        = qde_fundos_com_sfbp, 
                  "Com balanço patrimonial apenas"      = qde_fundos_so_bp,
                  "Com balanço patrimonial, não pertencentes ao OFSS" = qde_fundos_com_bp_nofss,
                  "Sem recursos financeiros" = qde_fundos_sem_fin)

tipos_fundos <- names(lista_fundos)

vlrs_fundos  <- data.frame(
  "Tipo_fundo" = tipos_fundos,
  "Valores"    = c(48.7, 175.4, 52.1, 0, 0)
)

cria_df <- function(item_fundos) {
  df <- data.frame(
    "Sequencial_tipo" = 1:item_fundos,
    "Tipo_fundo"      = rep(names(item_fundos), item_fundos) # (1)
  )
  return(df)
}

## Obs: (1) no map, ele não passa os "names" do vetor.


# a <- cria_df(lista_fundos[2])

## teste map

  # cria_df2 <- function(item_fundos, item_tipos) {
  #   df <- data.frame(
  #     "Sequencial_tipo" = 1:item_fundos,
  #     "Tipo_fundo"      = rep(item_nomes, item_fundos)
  #   )
  #   return(df)
  # }
  # 
  # lista_map <- map2(lista_fundos, tipos_fundos, cria_df2)

## fim teste map
  

lista <- list()

for (i in 1:length((lista_fundos))) {
  lista[[i]] <- cria_df(lista_fundos[i])
}

base_fundos <- bind_rows(lista)

largura_plot <- 4
padding_entre_grupos <- 1.5

qde_linhas_por_grupo <- ((lista_fundos-1)%/%largura_plot) + 1

### observar a diferença sutil entre os dois vetores abaixo!
#lag(cumsum(qde_linhas_por_grupo), 1, default = 0)
#((lag(cumsum(lista_fundos), 1, default = 0) - 1) %/% largura_plot) + 1

tabela_padding <- data.frame(
  Tipo_fundo   = tipos_fundos,
  Padding      = 1:length(tipos_fundos) * padding_entre_grupos,
  pos_inicial  = lag(cumsum(qde_linhas_por_grupo), 1, default = 0)
)


fundos <- base_fundos %>%
  left_join(vlrs_fundos) %>%
  mutate(x = ((Sequencial_tipo - 1) %%  largura_plot) + 1,
         y = ((Sequencial_tipo - 1) %/% largura_plot) + 1) %>%
  left_join(tabela_padding) %>%
  mutate(y = y + pos_inicial + Padding)

# calculo das posições médias, para plotar bolhas

pos_medias <- fundos %>%
  group_by(Tipo_fundo) %>%
  summarise(y_med = (min(y)+max(y))/2,
            valores_text = first(Valores),
            valores = ifelse(valores_text == 0, NA, valores_text),
            qde = n())



grafico <- ggplot(fundos, aes(x = x, y = y, fill = Tipo_fundo)) + 
  #geom_point(aes(color = Tipo_fundo), size = 5) + 
  geom_tile(color = "white", width = 0.75, height = 0.75) + 
  geom_point(data = pos_medias, aes(x = 26, y = y_med, size = valores, color = Tipo_fundo)) +
  geom_text(data = pos_medias, 
            aes(x = 26, 
                y = ifelse(
                  Tipo_fundo %in% c("Sem recursos financeiros",
                                    "Com balanço patrimonial, não pertencentes ao OFSS"), 
                  NA, 
                  y_med), 
                label = paste0("R$ ", valores_text, " bi", 
                               ifelse(Tipo_fundo == "Com balanço patrimonial apenas", "*", ""))),
            hjust = "center", vjust = "center", size = 3.5, family = "Source Sans Pro",
            color = "white") + 
  geom_text(data = pos_medias,
            aes(x = 6, y = y_med-0.3, color = Tipo_fundo,
                label = Tipo_fundo),
             hjust = "left", vjust = "bottom", size = 4, family = "Source Sans Pro", fontface = "bold") +
  geom_text(data = pos_medias,
            aes(x = 6, y = y_med+0.3, color = Tipo_fundo,
                label = paste0(qde, " fundos")),
            hjust = "left", vjust = "top", size = 4, family = "Source Sans Pro") +
  scale_x_continuous(limits = c(NA, 28)) +
  #scale_size(range = c(0, 20)) +
  scale_size_area(max_size = 43) +
  scale_y_reverse() +
  scale_color_manual(values = c("Sem recursos financeiros"         = "#999999", 
                                "Com superávit financeiro apenas"  = "#277E8E", 
                                "Com superávit financeiro e balanço patrimonial" = "#5E733B", 
                                "Com balanço patrimonial apenas"      = "#7D7213",
                                "Com balanço patrimonial, não pertencentes ao OFSS" = "#888888")) +
  scale_fill_manual(values = c("Sem recursos financeiros"         = "#999999", 
                                "Com superávit financeiro apenas"  = "#277E8E", 
                                "Com superávit financeiro e balanço patrimonial" = "#5E733B", 
                                "Com balanço patrimonial apenas"      = "#7D7213",
                               "Com balanço patrimonial, não pertencentes ao OFSS" = "#888888")) +  
  labs(caption = "* Valor das disponibilidades líquidas (recursos líquidos deduzidos \ndos passivos circulantes) apuradas nos Balanços Patrimoniais desses fundos") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        text = element_text(family = "Source Sans Pro", 
                            color = "#555555"),
        plot.caption = element_text(color = "#333333", size = 10),
        plot.title = element_text(family = "Source Sans Pro", face = "bold", hjust = "0.5"))

ggsave(grafico, file = "plot.PNG", height = 8.5, width = 7, dpi = 300, type = "cairo")
