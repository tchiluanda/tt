library(tidyverse)
library(extrafont)
library(readxl)
library(zoo)
library(viridis)

dados_originais <- read_excel("aplic_rec_emissoes.xlsx", skip = 8)
meses <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
MESES <- str_to_upper(meses)

dados <- dados_originais %>%
  rename(mes = 1, cod_grupo = 2, nom_grupo = 3, valor = 4) %>%
  mutate(grupo = paste(cod_grupo, "-", nom_grupo)) %>%
  separate(mes, into = c("mes", "ano"), sep = "/") %>%
  filter(!(mes %in% c("013", "014", "000"))) %>%
  mutate(mes_num = match(mes, MESES),
         periodo = as.Date(paste(ano, str_pad(mes_num, 2, pad = "0"), "01", sep="-"))) %>%
  select(periodo, grupo, valor) %>%
  spread(grupo, valor, fill = 0) %>%
  mutate_at(vars(-periodo), 
            funs(zoo::rollapply(as.numeric(.), 
                                width = 12, 
                                FUN = sum, 
                                fill = NA, align = 'right')
                 )) %>%
  gather(-1, key = "grupo", value = "valor") %>%
  filter(!is.na(valor))

ordem <- data.frame(
  grupo = c("1 - PESSOAL E ENCARGOS SOCIAIS", "2 - JUROS E ENCARGOS DA DIVIDA", 
            "3 - OUTRAS DESPESAS CORRENTES", "4 - INVESTIMENTOS", "5 - INVERSOES FINANCEIRAS", 
            "6 - AMORTIZACAO/REFINANCIAMENTO DA DIVIDA"),
  ordem = c(6,2,5,3,4,1)
)

dados_ordenados <- dados %>%
  left_join(ordem) %>%
  arrange(ordem, periodo) %>%
  mutate(grupo = factor(grupo, levels = unique(dados$grupo)[c(6,2,5,3,4,1)]))

tema <- function(){
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro", colour = "grey20"),
      title = element_text(face = "bold", size = 10, color = "#1E4C7A"), 
      plot.subtitle = element_text(family = "Source Sans Pro", 
                                   color = "grey20", face = "plain", size = 10),
      axis.text = element_text(family = "Source Sans Pro", colour = "grey40", size = 12),
      plot.caption = element_text(face = "italic"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      axis.ticks = element_line(size = 0.5, color = "grey40"),
      axis.ticks.length = unit(.15, "cm"),
      axis.title = element_text(size = 8, colour = "grey20"),
      legend.position = 'none')
}

ggplot(dados_ordenados, aes(x = periodo, y = valor, fill = grupo)) + 
  geom_area() + 
  theme(legend.position = "bottom") +
  scale_fill_viridis_d() + 
  scale_y_continuous(labels = function(x){format(x/1e9, big.mark = ".", decimal.mark = ",")}) +
  labs(x = NULL, y = "R$ bi") +
  tema()

a <- b >= c 


