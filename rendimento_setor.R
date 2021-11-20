rm(list = ls())

## Scrit formulado por Victor Nobre, Ledson Gomes e Igor Soares

### Este script serve para manipular as bases do IBGE, referente aos rendimentos por setor

# carregando pacotes

library(PNADcIBGE)
library(tidyverse)
#library(descr)
library(survey)
#library(sidrar)
library(data.table)

# baixando as bases

var_select <- c("VD4019", "VD3004", "VD4010", "VD4009", "VD4012")

pnad_2019 <- get_pnadc(year = 2019, quarter = 4, vars = var_select)
pnad_2020_1 <- get_pnadc(year = 2020, quarter = 1, vars = var_select)
pnad_2020_2 <- get_pnadc(year = 2020, quarter = 2, vars = var_select)
pnad_2020_3 <- get_pnadc(year = 2020, quarter = 3, vars = var_select)
pnad_2020_4 <- get_pnadc(year = 2020, quarter = 4, vars = var_select)
pnad_2021_1 <- get_pnadc(year = 2021, quarter = 1, vars = var_select)
pnad_2021_2 <- get_pnadc(year = 2021, quarter = 2, vars = var_select)


# Combinando categorias - para compatibilização com o PIB Trimestral

a <- pnad_2019
e <-a$variables$VD4010[21] # "Administração pública, defesa e seguridade social "
f <-a$variables$VD4010[30] # "Outros Serviços"
aea <-a$variables$VD4010[10] # "Alojamento e alimentação"


pnad_2019$variables$VD4010[pnad_2019$variables$VD4010 == "Educação, saúde humana e serviços sociais"] <- as.factor(e)
pnad_2019$variables$VD4010[pnad_2019$variables$VD4010 == "Serviços domésticos"] <- as.factor(f)
pnad_2019$variables$VD4010[pnad_2019$variables$VD4010 == aea] <- as.factor(f) 

pnad_2020_1$variables$VD4010[pnad_2020_1$variables$VD4010 == "Educação, saúde humana e serviços sociais"] <- as.factor(e)
pnad_2020_1$variables$VD4010[pnad_2020_1$variables$VD4010 == "Serviços domésticos"] <- as.factor(f)
pnad_2020_1$variables$VD4010[pnad_2020_1$variables$VD4010 == aea] <- as.factor(f) 

pnad_2020_2$variables$VD4010[pnad_2020_2$variables$VD4010 == "Educação, saúde humana e serviços sociais"] <- as.factor(e)
pnad_2020_2$variables$VD4010[pnad_2020_2$variables$VD4010 == "Serviços domésticos"] <- as.factor(f)
pnad_2020_2$variables$VD4010[pnad_2020_2$variables$VD4010 == aea] <- as.factor(f) 

pnad_2020_3$variables$VD4010[pnad_2020_3$variables$VD4010 == "Educação, saúde humana e serviços sociais"] <- as.factor(e)
pnad_2020_3$variables$VD4010[pnad_2020_3$variables$VD4010 == "Serviços domésticos"] <- as.factor(f)
pnad_2020_3$variables$VD4010[pnad_2020_3$variables$VD4010 == aea] <- as.factor(f) 

pnad_2020_4$variables$VD4010[pnad_2020_4$variables$VD4010 == "Educação, saúde humana e serviços sociais"] <- as.factor(e)
pnad_2020_4$variables$VD4010[pnad_2020_4$variables$VD4010 == "Serviços domésticos"] <- as.factor(f)
pnad_2020_4$variables$VD4010[pnad_2020_4$variables$VD4010 == aea] <- as.factor(f)

pnad_2021_1$variables$VD4010[pnad_2021_1$variables$VD4010 == "Educação, saúde humana e serviços sociais"] <- as.factor(e)
pnad_2021_1$variables$VD4010[pnad_2021_1$variables$VD4010 == "Serviços domésticos"] <- as.factor(f)
pnad_2021_1$variables$VD4010[pnad_2021_1$variables$VD4010 == aea] <- as.factor(f) 

pnad_2021_2$variables$VD4010[pnad_2021_2$variables$VD4010 == "Educação, saúde humana e serviços sociais"] <- as.factor(e)
pnad_2021_2$variables$VD4010[pnad_2021_2$variables$VD4010 == "Serviços domésticos"] <- as.factor(f)
pnad_2021_2$variables$VD4010[pnad_2021_2$variables$VD4010 == aea] <- as.factor(f) 
rm(a,e,f,aea,var_select)

## RENDIMENTO MEDIO POR SETOR ##

pnad_rendimento_medio_setor_2019 <- svyby(formula =~VD4019, by = ~VD4010, design = pnad_2019, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_setor_2020_1 <- svyby(formula =~VD4019, by = ~VD4010, design = pnad_2020_1, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_setor_2020_2 <- svyby(formula =~VD4019, by = ~VD4010, design = pnad_2020_2, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_setor_2020_3 <- svyby(formula =~VD4019, by = ~VD4010, design = pnad_2020_3, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_setor_2020_4 <- svyby(formula =~VD4019, by = ~VD4010, design = pnad_2020_4, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_setor_2021_1 <- svyby(formula =~VD4019, by = ~VD4010, design = pnad_2021_1, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_setor_2021_2 <- svyby(formula =~VD4019, by = ~VD4010, design = pnad_2021_2, FUN = svymean, na.rm = TRUE )

# adicionar os trimestres

pnad_rendimento_medio_setor_2019 <- pnad_rendimento_medio_setor_2019 %>% mutate(trimestre = "2019/4T")
pnad_rendimento_medio_setor_2020_1 <- pnad_rendimento_medio_setor_2020_1 %>% mutate(trimestre = "2020/1T")
pnad_rendimento_medio_setor_2020_2 <- pnad_rendimento_medio_setor_2020_2 %>% mutate(trimestre = "2020/2T")
pnad_rendimento_medio_setor_2020_3 <- pnad_rendimento_medio_setor_2020_3 %>% mutate(trimestre = "2020/3T")
pnad_rendimento_medio_setor_2020_4 <- pnad_rendimento_medio_setor_2020_4 %>% mutate(trimestre = "2020/4T")
pnad_rendimento_medio_setor_2021_1 <- pnad_rendimento_medio_setor_2021_1 %>% mutate(trimestre = "2021/1T")
pnad_rendimento_medio_setor_2021_2 <- pnad_rendimento_medio_setor_2021_2 %>% mutate(trimestre = "2021/2T")

# agregando as bases de rendimento medio

pnad_rendimento_agregado_setor <- rbindlist(list(pnad_rendimento_medio_setor_2019,
                                                 pnad_rendimento_medio_setor_2020_1,
                                                 pnad_rendimento_medio_setor_2020_2,
                                                 pnad_rendimento_medio_setor_2020_3,
                                                 pnad_rendimento_medio_setor_2020_4,
                                                 pnad_rendimento_medio_setor_2021_1,
                                                 pnad_rendimento_medio_setor_2021_2))



pnad_rendimento_agregado_setor <- pnad_rendimento_agregado_setor[,-"se"]

# organizar dataframe

pnad_rendimento_agregado_setor <- pnad_rendimento_agregado_setor %>% select(trimestre,instrucao, setor, VD4019)

pnad_rendimento_agregado_setor <- pnad_rendimento_agregado_setor %>% rename(rendimento_medio_nominal = VD4019)

pnad_rendimento_agregado_setor$rendimento_medio_nominal <- round(pnad_rendimento_agregado_setor$rendimento_medio_nominal, digits = 2)

# ---- exportando a base no R ---- 
write.csv(pnad_rendimento_agregado_setor, file = "pnad_rendimento_agregado_setor.csv")
