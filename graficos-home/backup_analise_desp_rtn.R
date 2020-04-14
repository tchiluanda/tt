## Despesas RTN

### Lê a planilha
```{r}
tabela_desp <- read_excel("rtn.xlsx", sheet = "1.4-A", skip = 4)
```

### Processa e plota os valores totais

```{r}

tabela_desp2 <- tabela_desp %>%
  rename(rotulos = 1) %>%
  filter(str_sub(rotulos,1,2) == "IV") %>%
  mutate_at(-1, .funs = ~as.numeric(.)) %>%
  mutate(total = rowSums(.[-1])) %>%
  select(rotulos, total) %>%
  #arrange(desc(total)) %>%
  filter(!is.na(total)) %>%
  mutate(class = str_extract(tabela_desp2$rotulos, "IV[\\.\\d]{1,}"),
         nivel = ifelse(str_sub(class, -1, -1) == ".", 
                        1,
                        str_count(class, "\\.") + 1)
  ) %>%
  separate(class, 
           into = paste("Nivel", 
                        1:max(.$nivel), 
                        sep = "_"),
           sep = "\\.",
           remove = FALSE) %>%
  rowwise() %>%
  mutate(ultimo_nivel = ifelse(str_sub(class, -1, -1) == ".",
                               "Agregadora",
                               ifelse(sum(str_count(.$class, 
                                                    paste0(class,"\\."))) > 0,
                                      "Agregadora",
                                      "Ult_nivel")))

# caraca, suei pra fazer esse pipe... documentar!

plot1 <- ggplot(tabela_desp2 %>% filter(ultimo_nivel == "Ult_nivel"), aes(y = total, x = reorder(rotulos, total))) +
  geom_col() +
  coord_flip()

# conclusão: essa classificação é muito limitada. :/

serie_desp <- tabela_desp %>% 
  rename(rotulos = 1) %>%
  gather(-1, key = "Periodo", value = "Valor") %>%
  mutate(Valor = as.numeric(Valor),
         Periodo = as.Date(as.numeric(Periodo), origin = "1899-12-30"),
         data = paste0(lubridate::year(Periodo), "/", 
                       stringr::str_pad(lubridate::month(Periodo), width = 2, pad = "0"))) %>%
  select(-Periodo) %>%
  spread(key = data, value = Valor)

filter(!is.na(Valor_12m)) %>%
  filter(Periodo >= "2006-01-01") # para ficar igual à série da dívida