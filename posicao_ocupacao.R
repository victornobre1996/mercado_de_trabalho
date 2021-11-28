rm(list = ls())

## Scrit formulado por Victor Nobre, Ledson Gomes e Igor Soares

### Este script serve para manipular as bases do IBGE, pelos diferentes posicoes na ocupacao 

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

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
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



# numero de ocupados por setor e posicao na ocupacao 


=======
>>>>>>> parent of a268f07 (adicionando codigo para diferentes setores)
=======
>>>>>>> parent of a268f07 (adicionando codigo para diferentes setores)
=======
>>>>>>> parent of a268f07 (adicionando codigo para diferentes setores)
=======
>>>>>>> parent of a268f07 (adicionando codigo para diferentes setores)
pnad_2019_setor_ocupacao <- svyby(formula =~as.integer(VD4002 == "Pessoas ocupadas"),
                        by = ~interaction(VD4009,VD4010), design = pnad_2019, FUN=svytotal, keep.names=FALSE, na.rm=TRUE)

pnad_2020_1_setor_ocupacao <- svyby(formula =~as.integer(VD4002 == "Pessoas ocupadas"),
                                  by = ~interaction(VD4009,VD4010), design = pnad_2020_1, FUN=svytotal, keep.names=FALSE, na.rm=TRUE)

pnad_2020_2_setor_ocupacao <- svyby(formula =~as.integer(VD4002 == "Pessoas ocupadas"),
                                    by = ~interaction(VD4009,VD4010), design = subset(pnad_2020_2,V2009 >=14), FUN=svytotal, keep.names=FALSE, na.rm=TRUE)


pnad_2020_3_setor_ocupacao <- svyby(formula =~as.integer(VD4002 == "Pessoas ocupadas"),
                                    by = ~interaction(VD4009,VD4010), design = subset(pnad_2020_3,V2009 >=14), FUN=svytotal, keep.names=FALSE, na.rm=TRUE)


pnad_2020_4_setor_ocupacao <- svyby(formula =~as.integer(VD4002 == "Pessoas ocupadas"),
                                    by = ~interaction(VD4009,VD4010), design = subset(pnad_2020_4,V2009 >=14), FUN=svytotal, keep.names=FALSE, na.rm=TRUE)


pnad_2021_1_setor_ocupacao <- svyby(formula =~as.integer(VD4002 == "Pessoas ocupadas"),
                                    by = ~interaction(VD4009,VD4010), design = subset(pnad_2021_1,V2009 >=14), FUN=svytotal, keep.names=FALSE, na.rm=TRUE)

pnad_2021_2_setor_ocupacao <- svyby(formula =~as.integer(VD4002 == "Pessoas ocupadas"),
                                    by = ~interaction(VD4009,VD4010), design = subset(pnad_2021_2,V2009 >=14), FUN=svytotal, keep.names=FALSE, na.rm=TRUE)



pnad_setor_ocupacao_setor <- rbindlist(list(pnad_rendimento_medio_setor_2019,
                                                 pnad_rendimento_medio_setor_2020_1,
                                                 pnad_rendimento_medio_setor_2020_2,
                                                 pnad_rendimento_medio_setor_2020_3,
                                                 pnad_rendimento_medio_setor_2020_4,
                                                 pnad_rendimento_medio_setor_2021_1,
                                                 pnad_rendimento_medio_setor_2021_2))





