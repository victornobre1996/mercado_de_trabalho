rm(list = ls())

## Scrit formulado por Victor Nobre, Ledson Gomes e Igor Soares

### Este script serve para manipular as bases do IBGE, referente aos rendimentos por setor

# carregando pacotes

library(PNADcIBGE)
library(tidyverse)
library(survey)
library(data.table)

# baixando as bases


var_select <- c("VD4002", "V2009", "VD4019", "VD4020","VD3004", "VD4010", 
                "VD4008", "VD4009", "VD4011" ,"VD4012")


pnad_2019 <- get_pnadc(year = 2019, quarter = 4, vars = var_select, deflator = T)
pnad_2020_1 <- get_pnadc(year = 2020, quarter = 1, vars = var_select, deflator = T)
pnad_2020_2 <- get_pnadc(year = 2020, quarter = 2, vars = var_select, deflator = T)
pnad_2020_3 <- get_pnadc(year = 2020, quarter = 3, vars = var_select, deflator = T)
pnad_2020_4 <- get_pnadc(year = 2020, quarter = 4, vars = var_select, deflator = T)
pnad_2021_1 <- get_pnadc(year = 2021, quarter = 1, vars = var_select, deflator = T)
pnad_2021_2 <- get_pnadc(year = 2021, quarter = 2, vars = var_select, deflator = T)
pnad_2021_3 <- get_pnadc(year = 2021, quarter = 3, vars = var_select, deflator = T)

# Combinando categorias - para compatibilização com o PIB Trimestral

##Classificações de Referência##
a <- pnad_2019
e <-a$variables$VD4010[21] # "Administração pública, defesa e seguridade social "
h <-a$variables$VD4010[40] # "Informação, comunicação e atividades financeiras, 
#imobiliárias, profissionais e administrativas"
f <-a$variables$VD4010[30] # "Outros Serviços"
g <-a$variables$VD4008[1] # "Empregado no Setor Privado"
aea <-a$variables$VD4010[10] # "Alojamento e alimentação"
r <- pnad_2020_1$variables$VD4010[204] # "Atividades Mal Definidas"

mylist <- list(pnad_2019, pnad_2020_1, pnad_2020_2, pnad_2020_3,
               pnad_2020_4, pnad_2020_1, pnad_2021_2, pnad_2021_3)
mylist_1 <- list("2019","2020_1", "2020_2",
                 "2020_3", "2020_4",
                 "2021_1", "2021_2","2021_3")

#Loop para a Substituição
for (W in seq_along(mylist)){
  
  a <- mylist[[W]]
  
  # ***Separando Emprego Público e Privado em Adm. Pública(etc) e Outros (respectivamente) ***
  a$variables$VD4010[a$variables$VD4010 == "Educação, saúde humana e serviços sociais" &
                       a$variables$VD4008 == g ] <- as.factor(f)
  a$variables$VD4010[a$variables$VD4010 == "Educação, saúde humana e serviços sociais" &
                       a$variables$VD4008 != g ] <- as.factor(e)
  # *** Agregando categorias em Outros ***
  a$variables$VD4010[a$variables$VD4010 == h & 
                       a$variables$VD4011 == "Diretores e gerentes" & 
                       a$variables$VD4011 == "Profissionais das ciências e intelectuais"&
                       a$variables$VD4011 == "Técnicos e profissionais de nível médio"&
                       a$variables$VD4011 == "Trabalhadores de apoio administrativo"] <- as.factor(f)
  a$variables$VD4010[a$variables$VD4010 == "Serviços domésticos"] <- as.factor(f)
  a$variables$VD4010[a$variables$VD4010 == aea] <- as.factor(f)
  a$variables$VD4010[a$variables$VD4010 == r] <- as.factor(f)
  
  assign(paste0("pnad_",mylist_1[[W]]),a);
}
rm(r,a,e,f,aea,h,g,var_select)

## RENDIMENTO MEDIO - Total Trimestre ##

pnad_rendimento_medio_total_2019 <- svyby(formula =~(VD4020_real =VD4020*Efetivo), by = ~Ano, design = pnad_2019, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_total_2020_1 <- svyby(formula =~(VD4020_real =VD4020*Efetivo), by = ~Ano, design = pnad_2020_1, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_total_2020_2 <- svyby(formula =~(VD4020_real =VD4020*Efetivo), by = ~Ano, design = pnad_2020_2, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_total_2020_3 <- svyby(formula =~(VD4020_real =VD4020*Efetivo), by = ~Ano, design = pnad_2020_3, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_total_2020_4 <- svyby(formula =~(VD4020_real =VD4020*Efetivo), by = ~Ano, design = pnad_2020_4, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_total_2021_1 <- svyby(formula =~(VD4020_real =VD4020*Efetivo), by = ~Ano, design = pnad_2021_1, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_total_2021_2 <- svyby(formula =~(VD4020_real =VD4020*Efetivo), by = ~Ano, design = pnad_2021_2, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_total_2021_3 <- svyby(formula =~(VD4020_real =VD4020*Efetivo), by = ~Ano, design = pnad_2021_3, FUN = svymean, na.rm = TRUE )

# adicionar os trimestres e organizar dataframe

mylist_1 <-list("2019/4T", "2020/1T", "2020/2T", "2020/3T",
                "2020/4T", "2021/1T", "2021/2T", "2021/3T")
mylist_2 <- list("2019","2020_1", "2020_2",
                 "2020_3", "2020_4",
                 "2021_1", "2021_2","2021_3")


l <- list(pnad_rendimento_medio_total_2019,
          pnad_rendimento_medio_total_2020_1,
          pnad_rendimento_medio_total_2020_2,
          pnad_rendimento_medio_total_2020_3,
          pnad_rendimento_medio_total_2020_4,
          pnad_rendimento_medio_total_2021_1,
          pnad_rendimento_medio_total_2021_2,
          pnad_rendimento_medio_total_2021_3)

for (i in seq_along(l)) {
  a <- l[[i]]
  a$trimestre <- c(mylist_1[[i]])
  a <- a %>% select(-"Ano", -"se")
  assign(paste0("pnad_rendimento_medio_total_",mylist_2[[i]]), a);
  
}

pnad_rendimento_medio_total <-rbindlist(list(pnad_rendimento_medio_total_2019,
                                             pnad_rendimento_medio_total_2020_1,
                                             pnad_rendimento_medio_total_2020_2,
                                             pnad_rendimento_medio_total_2020_3,
                                             pnad_rendimento_medio_total_2020_4,
                                             pnad_rendimento_medio_total_2021_1,
                                             pnad_rendimento_medio_total_2021_2,
                                             pnad_rendimento_medio_total_2021_3), use.names = F)

pnad_rendimento_medio_total <- pnad_rendimento_medio_total %>% rename(rendimento_medio_real = "VD4019_real = VD4019 * Habitual")

pnad_rendimento_medio_total$setor <- "Total"

pnad_rendimento_medio_total <- pnad_rendimento_medio_total %>% select(trimestre, setor ,rendimento_medio_real)

pnad_rendimento_medio_total$rendimento_medio_real <- round(pnad_rendimento_medio_total$rendimento_medio_real, digits = 2)



## RENDIMENTO MEDIO POR SETOR ##

pnad_rendimento_medio_setor_2019 <- svyby(formula =~(VD4020_real =VD4020*Efetivo), by = ~VD4010, design = pnad_2019, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_setor_2020_1 <- svyby(formula =~(VD4020_real =VD4020*Efetivo), by = ~VD4010, design = pnad_2020_1, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_setor_2020_2 <- svyby(formula =~(VD4020_real =VD4020*Efetivo), by = ~VD4010, design = pnad_2020_2, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_setor_2020_3 <- svyby(formula =~(VD4020_real =VD4020*Efetivo), by = ~VD4010, design = pnad_2020_3, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_setor_2020_4 <- svyby(formula =~(VD4020_real =VD4020*Efetivo), by = ~VD4010, design = pnad_2020_4, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_setor_2021_1 <- svyby(formula =~(VD4020_real =VD4020*Efetivo), by = ~VD4010, design = pnad_2021_1, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_setor_2021_2 <- svyby(formula =~(VD4020_real =VD4020*Efetivo), by = ~VD4010, design = pnad_2021_2, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_setor_2021_3 <- svyby(formula =~(VD4020_real =VD4020*Efetivo), by = ~VD4010, design = pnad_2021_3, FUN = svymean, na.rm = TRUE )

# adicionar os trimestres

pnad_rendimento_medio_setor_2019 <- pnad_rendimento_medio_setor_2019 %>% mutate(trimestre = "2019/4T")
pnad_rendimento_medio_setor_2020_1 <- pnad_rendimento_medio_setor_2020_1 %>% mutate(trimestre = "2020/1T")
pnad_rendimento_medio_setor_2020_2 <- pnad_rendimento_medio_setor_2020_2 %>% mutate(trimestre = "2020/2T")
pnad_rendimento_medio_setor_2020_3 <- pnad_rendimento_medio_setor_2020_3 %>% mutate(trimestre = "2020/3T")
pnad_rendimento_medio_setor_2020_4 <- pnad_rendimento_medio_setor_2020_4 %>% mutate(trimestre = "2020/4T")
pnad_rendimento_medio_setor_2021_1 <- pnad_rendimento_medio_setor_2021_1 %>% mutate(trimestre = "2021/1T")
pnad_rendimento_medio_setor_2021_2 <- pnad_rendimento_medio_setor_2021_2 %>% mutate(trimestre = "2021/2T")
pnad_rendimento_medio_setor_2021_3 <- pnad_rendimento_medio_setor_2021_3 %>% mutate(trimestre = "2021/3T")

# agregando as bases de rendimento medio

pnad_rendimento_agregado_setor <- rbindlist(list(pnad_rendimento_medio_setor_2019,
                                                 pnad_rendimento_medio_setor_2020_1,
                                                 pnad_rendimento_medio_setor_2020_2,
                                                 pnad_rendimento_medio_setor_2020_3,
                                                 pnad_rendimento_medio_setor_2020_4,
                                                 pnad_rendimento_medio_setor_2021_1,
                                                 pnad_rendimento_medio_setor_2021_2,
                                                 pnad_rendimento_medio_setor_2021_3), use.names = F)



pnad_rendimento_agregado_setor <- pnad_rendimento_agregado_setor[,-"se"]

# organizar dataframe
pnad_rendimento_agregado_setor <- pnad_rendimento_agregado_setor %>% rename(setor = VD4010)

pnad_rendimento_agregado_setor <- pnad_rendimento_agregado_setor %>% rename(rendimento_medio_real = "VD4019_real = VD4019 * Habitual")

pnad_rendimento_agregado_setor <- pnad_rendimento_agregado_setor %>% select(trimestre, setor, rendimento_medio_real )

pnad_rendimento_agregado_setor$rendimento_medio_real <- round(pnad_rendimento_agregado_setor$rendimento_medio_real, digits = 2)

# agregando as todas as bases

pnad_rendimento_agregado_setor_total <- rbindlist(list(pnad_rendimento_agregado_setor,
                                                       pnad_rendimento_medio_total), use.names = F)


# ---- exportando a base no R ---- 
write.csv(pnad_rendimento_agregado_setor_total, file = "pnad_rendimento_agregado_setor_efetivo.csv")
