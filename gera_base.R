### Script para gerar a base que é disponibilizada no TT/ckan

library(readxl)
library(stringr)
library(lubridate)
library(tidyverse)

# montando uma base anterior ----------------------------------------------

arquivos_exceto_RGPS <- list.files(path = "./dados", pattern = "*ex RGPS Pgtos Totais Esf-Acao-Mod.xlsx", full.names = T)

arquivos_apenas_RGPS <- list.files(path = "./dados", pattern = "*so RGPS Pgtos Totais Esf-Acao-Mod.xlsx", full.names = T)

## removendo temporariamente o exercício atual

exercicio_atual <- as.character(year(Sys.Date()))

### achando a posição do exercício atual na lista de arquivos exceto RGPS
vetor_pos_exRGPS <- str_detect(arquivos_exceto_RGPS, exercicio_atual)

arq_atual_exceto_RGPS <- arquivos_exceto_RGPS[ vetor_pos_exRGPS]
arquivos_exceto_RGPS  <- arquivos_exceto_RGPS[!vetor_pos_exRGPS]

### achando a posição do exercício atual na lista de arquivos apenas RGPS
vetor_pos_soRGPS <- str_detect(arquivos_apenas_RGPS, exercicio_atual)

arq_atual_exceto_RGPS <- arquivos_exceto_RGPS[ vetor_pos_soRGPS]
arquivos_exceto_RGPS  <- arquivos_exceto_RGPS[!vetor_pos_soRGPS]


# gerando as bases fechadas -----------------------------------------------

base_fechada_ex_RGPS <- purrr::map(.x = arquivos_exceto_RGPS, .f = read_excel, skip = 9) %>%
  bind_rows()

base_fechada_so_RGPS <- purrr::map(.x = arquivos_apenas_RGPS, .f = read_excel, skip = 9) %>%
  bind_rows()

# cria classificador para indicar se a despesa é ou não do RGPS
base_fechada_ex_RGPS$RGPS <- FALSE
base_fechada_so_RGPS$RGPS  <- TRUE

# junta as duas tabelas
base_fechada <- bind_rows(base_fechada_ex_RGPS, base_fechada_so_RGPS)

# liberando um pouco de recursos
base_fechada_so_RGPS <- NULL
base_fechada_ex_RGPS <- NULL

save(base_fechada, file = "base_fechada.RData")

##### com sorte, não precisa mais fazer esse pedaço de cima.

dados_desp_RGPS <- NULL

# arrumando um pouco
colnames(dados_desp) <- c("Esfera_cod",
                          "Esfera_nome",
                          "Funcao_cod",
                          "Funcao_nome",
                          "Subfuncao_cod",
                          "Subfuncao_nome",
                          "Programa_cod",
                          "Programa_nome",
                          "Acao_cod",
                          "Acao_nome",
                          "GND_cod",
                          "GND_nome",
                          "Modalidade_id",
                          "Modalidade_cod",
                          "Modalidade_nome",
                          "Elemento_id",
                          "Elemento_cod",
                          "Elemento_nome",
                          "Fonte_id",
                          "Fonte_cod",
                          "Fonte_nome",
                          "ResultadoPrim_cod",
                          "ResultadoPrim_nome",
                          "ExcecaoProgFin_cod",
                          "Ano",
                          "Mes",
                          "Periodo",
                          "ItemInfo_id",
                          "ItemInfo_nome",
                          "ItemInfo_cod",
                          "SaldoAnterior",
                          "SaldoAtual",
                          "DespesaRGPS")

dados <- dados_desp %>%
  filter(!(Mes %in% c(13, 14))) %>%
  mutate(Periodo = as.Date(paste0(Ano, "-",
                                  str_pad(Mes, width = 2, pad = "0"), "-",
                                  "01")),
         SaldoAnterior = as.numeric(as.character(SaldoAnterior)),
         SaldoAtual = as.numeric(as.character(SaldoAtual))) %>%
  replace_na(list(SaldoAnterior = 0, SaldoAtual = 0)) %>%
  mutate(Valor = round(SaldoAtual - SaldoAnterior, 2)) %>%
  filter(!(Funcao_cod %in% c("-7","-9")) & 
           !(GND_cod %in% c("-7","-9")) &
           !(Modalidade_cod %in% c("-7","-9"))) %>%
  select(-SaldoAtual, -SaldoAnterior, -ends_with("_id"))

dados_desp %>% 
  write.csv2("./dados/despUniao.csv", row.names = FALSE)

