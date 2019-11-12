library(tidyverse)

dados_brutos <- read.csv2("alugueis2011_2019_real.csv")

dados <- dados_brutos %>%
  mutate(saldo = as.numeric(as.character(SALDORITEMINFORMAODIALANAMENT)),
         tipo_doc = str_sub(ID_DOCUMENTO_LANC, 16, 17)) %>%
  count(tipo_doc)
  
  group_by(ID_ANO_LANC, CO_NATUREZA_DESPESA_DETA) %>%
  summarise(saldo = sum(saldo)) %>%
  spread(key = ID_ANO_LANC, value = saldo)
  
