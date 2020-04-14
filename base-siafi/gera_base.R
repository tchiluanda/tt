### Script para gerar a base que é disponibilizada no TT/ckan

library(readxl)
library(writexl)
library(stringr)
library(lubridate)
library(tidyverse)


# listando os arquivos ----------------------------------------------------

arquivos_exceto_RGPS <- list.files(path = "./base-siafi/dados", pattern = "*ex RGPS Pgtos Totais Esf-Acao-Mod.xlsx", full.names = T)

arquivos_apenas_RGPS <- list.files(path = "./base-siafi/dados", pattern = "*so RGPS Pgtos Totais Esf-Acao-Mod.xlsx", full.names = T)

## removendo temporariamente o exercício atual

exercicio_atual <- as.character(year(Sys.Date()))
exercicio_atual <- "2019"

### achando a posição do exercício atual na lista de arquivos exceto RGPS
vetor_pos_exRGPS <- str_detect(arquivos_exceto_RGPS, exercicio_atual)

arq_atual_exceto_RGPS    <- arquivos_exceto_RGPS[ vetor_pos_exRGPS]
arq_fechado_exceto_RGPS  <- arquivos_exceto_RGPS[!vetor_pos_exRGPS]

### achando a posição do exercício atual na lista de arquivos apenas RGPS
vetor_pos_soRGPS <- str_detect(arquivos_apenas_RGPS, exercicio_atual)

arq_atual_apenas_RGPS    <- arquivos_apenas_RGPS[ vetor_pos_soRGPS]
arq_fechado_apenas_RGPS  <- arquivos_apenas_RGPS[!vetor_pos_soRGPS]


# gerando as bases fechadas -----------------------------------------------

# #=========================================================================#
# # só precisa fazer uma vez                                                #
# #=========================================================================#
# 
# base_fechada_ex_RGPS <- purrr::map(.x = arq_fechado_exceto_RGPS, .f = read_excel, skip = 9) %>%
#   bind_rows()
# 
# base_fechada_so_RGPS <- purrr::map(.x = arq_fechado_apenas_RGPS, .f = read_excel, skip = 9) %>%
#   bind_rows()
# 
# # cria classificador para indicar se a despesa é ou não do RGPS
# base_fechada_ex_RGPS$RGPS <- FALSE
# base_fechada_so_RGPS$RGPS  <- TRUE
# 
# # junta as duas tabelas
# base_fechada <- bind_rows(base_fechada_ex_RGPS, base_fechada_so_RGPS)
# 
# # liberando um pouco de recursos
# base_fechada_so_RGPS <- NULL
# base_fechada_ex_RGPS <- NULL
# 
# save(base_fechada, file = "base_fechada.RData")
# saveRDS(base_fechada, file = "./base-siafi/base_fechada.rds")
# 
# #=========================================================================#
# # com sorte, não precisa mais fazer esse pedaço de cima.                  #
# #=========================================================================#


# abrindo a base fechada --------------------------------------------------

# load("base_fechada.RData")

base_fechada <- read_rds("./base-siafi/base_fechada.rds")


# incorporando a base do ano ----------------------------------------------

base_atual_ex_RGPS <- read_excel(arq_atual_exceto_RGPS, skip = 9)
base_atual_so_RGPS <- read_excel(arq_atual_apenas_RGPS, skip = 9)

# criando as variáveis RGPS
base_atual_ex_RGPS$RGPS <- FALSE
base_atual_so_RGPS$RGPS  <- TRUE

# junta as duas tabelas atuais
base_atual <- bind_rows(base_atual_ex_RGPS, 
                        base_atual_so_RGPS)

# liberando um pouco de recursos
base_atual_so_RGPS <- NULL
base_atual_ex_RGPS <- NULL


# fechando a base completa e tratando os dados ----------------------------

base_completa <- bind_rows(base_atual,
                           base_fechada)

# liberando recursos
base_fechada <- NULL
base_atual   <- NULL


# atualizando a base fechada --------------------------------------------

# fiz isso agora quando 2019 fechou




# limpeza e arrumação dos dados -------------------------------------------

colnames(base_completa) <- c("Esfera_cod",
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

# verificação -- tem uma diferença em 2008, no exceto RGPS, no próprio TG.

base_completa %>% 
  #filter(Ano == "2019", as.numeric(Mes)<=4) %>% 
  group_by(Ano) %>% 
  summarise(total = as.character(sum(Valor))) 
  #%>% write.csv2(file = "verifica.csv")

# arrumação

dados <- base_completa %>%
  filter(!(Mes %in% c(13, 14))) %>%
  mutate(Periodo = as.Date(paste0(Ano, "-",
                                  str_pad(Mes, width = 2, pad = "0"), "-",
                                  "01"))) %>%
  filter(!(Funcao_cod %in% c("-7","-9")) & 
         !(GND_cod %in% c("-7","-9")) &
         !(Modalidade_cod %in% c("-7","-9")))


# dados %>% 
#   write.csv2("despesas_uniao.csv", row.names = FALSE)

save(dados, file = "despesa_uniao.RData")

dados %>%
  write_xlsx("despesa_uniao.xlsx")
