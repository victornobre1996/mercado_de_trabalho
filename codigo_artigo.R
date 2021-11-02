rm(list = ls())

### Este script serve para manipular as bases do IBGE

# carregando pacotes

library(PNADcIBGE)
library(tidyverse)
library(descr)
library(survey)
library(deflateBR)
library(sidrar)
library(stringr)

# baixando as bases

pnad_2019 <- get_pnadc(year = 2019, quarter = 4)
pnad_2020_1 <- get_pnadc(year = 2020, quarter = 1)
pnad_2020_2 <- get_pnadc(year = 2020, quarter = 2)
pnad_2020_3 <- get_pnadc(year = 2020, quarter = 3)
pnad_2020_4 <- get_pnadc(year = 2020, quarter = 4)
pnad_2021_1 <- get_pnadc(year = 2021, quarter = 1)
pnad_2021_2 <- get_pnadc(year = 2021, quarter = 2)

# RENDIMENTO MEDIO POR SETOR E NIVEL DE INSTRUCAO

pnad_rendimento_medio_2019 <- svyby(formula =~VD4019, by = ~interaction(VD3004,VD4010), design = pnad_2019, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_2020_1 <- svyby(formula =~VD4019, by = ~interaction(VD3004,VD4010), design = pnad_2020_1, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_2020_2 <- svyby(formula =~VD4019, by = ~interaction(VD3004,VD4010), design = pnad_2020_2, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_2020_3 <- svyby(formula =~VD4019, by = ~interaction(VD3004,VD4010), design = pnad_2020_3, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_2020_4 <- svyby(formula =~VD4019, by = ~interaction(VD3004,VD4010), design = pnad_2020_4, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_2021_1 <- svyby(formula =~VD4019, by = ~interaction(VD3004,VD4010), design = pnad_2021_1, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_2021_2 <- svyby(formula =~VD4019, by = ~interaction(VD3004,VD4010), design = pnad_2021_2, FUN = svymean, na.rm = TRUE )


# tratando a base 

pnad_rendimento_medio_2019 <- pnad_rendimento_medio_2019 %>% mutate(trimestre = "4T/2019")
pnad_rendimento_medio_2020_1 <- pnad_rendimento_medio_2020_1 %>% mutate(trimestre = "1T/2020")
pnad_rendimento_medio_2020_2 <- pnad_rendimento_medio_2020_2 %>% mutate(trimestre = "2T/2020")
pnad_rendimento_medio_2020_3 <- pnad_rendimento_medio_2020_3 %>% mutate(trimestre = "3T/2020")
pnad_rendimento_medio_2020_4 <- pnad_rendimento_medio_2020_4 %>% mutate(trimestre = "4T/2020")
pnad_rendimento_medio_2021_1 <- pnad_rendimento_medio_2021_1 %>% mutate(trimestre = "1T/2021")
pnad_rendimento_medio_2021_2 <- pnad_rendimento_medio_2021_2 %>% mutate(trimestre = "2T/2021")

# agregando as bases de rendimento medio

pnad_rendimento_agregado <- rbind(pnad_rendimento_medio_2019,
                                  pnad_rendimento_medio_2020_1,
                                  pnad_rendimento_medio_2020_2,
                                  pnad_rendimento_medio_2020_3,
                                  pnad_rendimento_medio_2020_4,
                                  pnad_rendimento_medio_2021_1,
                                  pnad_rendimento_medio_2021_2)



pnad_rendimento_agregado <- pnad_rendimento_agregado[,-1]

#---- transformando rowname para variavel ----

pnad_rendimento_agregado %>% 
  rownames_to_column(var = "model") -> pnad_rendimento_agregado 

# separando a variavel model

pnad_rendimento_agregado <- pnad_rendimento_agregado %>% separate(model, c("instrucao", "setor"), sep = "\\.")

# organizar dataframe

pnad_rendimento_agregado <- pnad_rendimento_agregado %>% select(trimestre,instrucao, setor, VD4019)

pnad_rendimento_agregado <- pnad_rendimento_agregado %>% rename(rendimento_medio_nominal = VD4019)

pnad_rendimento_agregado$rendimento_medio_nominal <- round(pnad_rendimento_agregado$rendimento_medio_nominal, digits = 2)

###
# pnad_rendimento_agregado$setor <- str_sub(pnad_rendimento_agregado$setor, 1, nchar(pnad_rendimento_agregado$setor)-1) 


# ---- exportando a base no R ---- 
write.csv(pnad_escolaridade_agregada, file = "Pnad_escolaridade_agregado.csv")

