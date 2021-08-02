### Script para gerar a base que é disponibilizada no TT/ckan

# copie os novos arquivos exportados do TG para "./base-siafi/dados"

library(readxl)
library(writexl)
library(stringr)
library(lubridate)
library(tidyverse)

# 1. base atual -----------------------------------------------------------

base_atual <- readRDS("./base-siafi/dados/exportados/despesa_uniao.rds")

# 2. dados mais recentes --------------------------------------------------

# lista arquivos

arquivo_exceto_RGPS <- list.files(path = "./base-siafi/dados", pattern = "*ex RGPS Pgtos Totais Esf-Acao-Mod.xlsx", full.names = T)

arquivo_apenas_RGPS <- list.files(path = "./base-siafi/dados", pattern = "*so RGPS Pgtos Totais Esf-Acao-Mod.xlsx", full.names = T)

# lê os arquivos
base_atualizacao_ex_RGPS <- read_excel(arquivo_exceto_RGPS, skip = 9)
base_atualizacao_so_RGPS <- read_excel(arquivo_apenas_RGPS, skip = 9)

# criando as variáveis RGPS
base_atualizacao_ex_RGPS$RGPS <- FALSE
base_atualizacao_so_RGPS$RGPS  <- TRUE

# junta as duas tabelas atuais
base_atualizacao <- bind_rows(
  base_atualizacao_ex_RGPS, 
  base_atualizacao_so_RGPS
  )

# liberando um pouco de recursos
rm(base_atualizacao_ex_RGPS, base_atualizacao_so_RGPS)



# 3. Limpeza e arrumação da base de atualização ---------------------------

colnames(base_atualizacao) <- c("Esfera_cod",
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
                             "Modalidade_cod",
                             "Modalidade_nome",
                             "ResultadoPrim_cod",
                             "ResultadoPrim_nome",
                             "ExcecaoProgFin_cod",
                             "Periodo",
                             "Ano",
                             "Mes",                             
                             "Valor",
                             "DespesaRGPS")

base_atualizacao_arrumada <- base_atualizacao %>%
  filter(!(Mes %in% c(13, 14))) %>%
  mutate(Periodo = as.Date(paste0(Ano, "-",
                                  str_pad(Mes, width = 2, pad = "0"), "-",
                                  "01"))) %>%
  filter(!(Funcao_cod %in% c("-7","-9")) & 
           !(GND_cod %in% c("-7","-9")) &
           !(Modalidade_cod %in% c("-7","-9")))

# 4. junta as duas bases para gerar nova base atual -----------------------

nova_base_atual <- bind_rows(
  base_atual,
  base_atualizacao_arrumada
)

# 4.5. Verifica totais ----------------------------------------------------

totais <- nova_base_atual %>% group_by(Ano) %>% summarise(Valor = sum(Valor))

# 5. exporta base, move arquivos de atualizacao ---------------------------

nova_base_atual %>%
  saveRDS(file = "./base-siafi/dados/exportados/despesa_uniao.rds")

# nova_base_atual %>%
#   write_xlsx("./base-siafi/despesa_uniao.xlsx")

nova_base_atual %>%
  write.csv(file = "./base-siafi/dados/exportados/despesa_uniao.csv", 
            fileEncoding = "UTF-8")

file.copy(
  c(arquivo_exceto_RGPS, arquivo_apenas_RGPS),
  "./base-siafi/dados/originais-processados/")

file.remove(
  c(arquivo_exceto_RGPS, arquivo_apenas_RGPS))

