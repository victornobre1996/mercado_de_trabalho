rm(list = ls())

## Scrit formulado por Victor Nobre, Ledson Gomes e Igor Soares

### Este script serve para manipular as bases do IBGE

# carregando pacotes

library(PNADcIBGE)
library(tidyverse)
#library(descr)
library(survey)
#library(sidrar)
library(data.table)

# baixando as bases

var_select <- c("VD4002", "V2009", "VD4019", "VD3004", "VD4010", 
                "VD4008", "VD4009", "VD4011" ,"VD4012")


pnad_2019 <- get_pnadc(year = 2019, quarter = 4, vars = var_select)
pnad_2020_1 <- get_pnadc(year = 2020, quarter = 1, vars = var_select)
pnad_2020_2 <- get_pnadc(year = 2020, quarter = 2, vars = var_select)
pnad_2020_3 <- get_pnadc(year = 2020, quarter = 3, vars = var_select)
pnad_2020_4 <- get_pnadc(year = 2020, quarter = 4, vars = var_select)
pnad_2021_1 <- get_pnadc(year = 2021, quarter = 1, vars = var_select)
pnad_2021_2 <- get_pnadc(year = 2021, quarter = 2, vars = var_select)


# Combinando categorias - para compatibilização com o PIB Trimestral

##Classificações de Referência##
a <- pnad_2019
e <-a$variables$VD4010[21] # "Administração pública, defesa e seguridade social "
h <-a$variables$VD4010[40] # "Informação, comunicação e atividades financeiras, 
#imobiliárias, profissionais e administrativas"
f <-a$variables$VD4010[30] # "Outros Serviços"
g <-a$variables$VD4008[1] # "Empregado no Setor Privado"
aea <-a$variables$VD4010[10] # "Alojamento e alimentação"

mylist <- list(pnad_2019, pnad_2020_1, pnad_2020_2, pnad_2020_3,
               pnad_2020_4, pnad_2020_1, pnad_2021_2)
mylist_1 <- list("2019","2020_1", "2020_2",
                 "2020_3", "2020_4",
                 "2021_1", "2021_2")

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
  
  assign(paste0("pnad_1",mylist_1[[W]]),a);
}
rm(a,e,f,aea,h,g,var_select)


## RENDIMENTO MEDIO POR SETOR E NIVEL DE INSTRUCAO ##  

pnad_rendimento_medio_2019 <- svyby(formula =~VD4019, by = ~interaction(VD3004,VD4010), design = pnad_2019, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_2020_1 <- svyby(formula =~VD4019, by = ~interaction(VD3004,VD4010), design = pnad_2020_1, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_2020_2 <- svyby(formula =~VD4019, by = ~interaction(VD3004,VD4010), design = pnad_2020_2, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_2020_3 <- svyby(formula =~VD4019, by = ~interaction(VD3004,VD4010), design = pnad_2020_3, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_2020_4 <- svyby(formula =~VD4019, by = ~interaction(VD3004,VD4010), design = pnad_2020_4, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_2021_1 <- svyby(formula =~VD4019, by = ~interaction(VD3004,VD4010), design = pnad_2021_1, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_2021_2 <- svyby(formula =~VD4019, by = ~interaction(VD3004,VD4010), design = pnad_2021_2, FUN = svymean, na.rm = TRUE )

# adicionar os trimestres

pnad_rendimento_medio_2019 <- pnad_rendimento_medio_2019 %>% mutate(trimestre = "2019/4T")
pnad_rendimento_medio_2020_1 <- pnad_rendimento_medio_2020_1 %>% mutate(trimestre = "2020/1T")
pnad_rendimento_medio_2020_2 <- pnad_rendimento_medio_2020_2 %>% mutate(trimestre = "2020/2T")
pnad_rendimento_medio_2020_3 <- pnad_rendimento_medio_2020_3 %>% mutate(trimestre = "2020/3T")
pnad_rendimento_medio_2020_4 <- pnad_rendimento_medio_2020_4 %>% mutate(trimestre = "2020/4T")
pnad_rendimento_medio_2021_1 <- pnad_rendimento_medio_2021_1 %>% mutate(trimestre = "2021/1T")
pnad_rendimento_medio_2021_2 <- pnad_rendimento_medio_2021_2 %>% mutate(trimestre = "2021/2T")


# agregando as bases de rendimento medio

pnad_rendimento_agregado <- rbindlist(list(pnad_rendimento_medio_2019,
                                           pnad_rendimento_medio_2020_1,
                                           pnad_rendimento_medio_2020_2,
                                           pnad_rendimento_medio_2020_3,
                                           pnad_rendimento_medio_2020_4,
                                           pnad_rendimento_medio_2021_1,
                                           pnad_rendimento_medio_2021_2))



pnad_rendimento_agregado <- pnad_rendimento_agregado[,-"se"]


#---- transformando rowname para variavel e separando ----

pnad_rendimento_agregado <- pnad_rendimento_agregado %>% 
  separate( 
    col = "interaction(VD3004, VD4010)", 
    into = c("instrucao", "setor"),
    sep = "\\.")

# organizar dataframe

pnad_rendimento_agregado <- pnad_rendimento_agregado %>% select(trimestre,instrucao, setor, VD4019)

pnad_rendimento_agregado <- pnad_rendimento_agregado %>% rename(rendimento_medio_nominal = VD4019)

pnad_rendimento_agregado$rendimento_medio_nominal <- round(pnad_rendimento_agregado$rendimento_medio_nominal, digits = 2)


# ---- exportando a base no R ---- 
write.csv(pnad_rendimento_agregado, file = "Pnad_rendimento_agregado.csv")






