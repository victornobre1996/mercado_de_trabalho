##Posição na Ocupação - Total da Economia ##

rm(list = ls())

## Scrit formulado por Victor Nobre, Ledson Gomes e Igor Soares

### Este script serve para manipular as bases do IBGE, pelos diferentes posicoes na ocupacao 

# carregando pacotes

library(PNADcIBGE)
library(tidyverse)
library(survey)
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
r <- pnad_2020_1$variables$VD4010[204] # "Atividades Mal Definidas"

##Definindo Listas##

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
  a$variables$VD4010[a$variables$VD4010 == r] <- as.factor(f)
  
  assign(paste0("pnad_",mylist_1[[W]]),a);
}
rm(r,a,e,f,aea,h,g,var_select)


#Ocupação - Total da Economia #

mylist_1 <-list("2019/4T", "2020/1T", "2020/2T", "2020/3T",
                "2020/4T", "2021/1T", "2021/2T")

for (i in seq_along(mylist)) {
  p <-mylist[[i]]
  a<-as.data.frame(summary(na.omit(
    interaction((p$variables$VD4010),
                (p$variables$VD4009),drop = T)))) %>% 
    mutate(trimestre = mylist_1[[i]])
  a <- a %>% mutate(row.names(a)) 
  
  a <- a %>% 
    separate( 
      col = "row.names(a)", 
      into = c("setor", "ocupacao"),
      sep = "\\.")
  
  a <- a[a$ocupacao != "Empregador" & a$ocupacao != "Conta-própria" ,]
  b <-as.data.frame(summary(na.omit(
    interaction((p$variables$VD4010),
                (p$variables$VD4009),
                (p$variables$VD4012),
                drop = T)))) %>% 
    mutate(trimestre = mylist_1[[i]])
  
  b <- b[-c(nrow(b)),]
  b <- b %>% mutate(row.names(b))
  b <- b %>% 
    separate( 
      col = "row.names(b)", 
      into = c("setor", "ocupacao", "PS"),
      sep = "\\.")
  
  b_1 <- b[b$ocupacao == "Empregador",]
  b_2 <- b[b$ocupacao == "Conta-própria",]
  b_c <- rbindlist(list(b_1,b_2))
  
  b <- b_c %>% unite(col = "row.names(b)", 
                     ocupacao:PS,
                     sep = " ")
  view(b)
  d <- rbindlist(list(a,b), use.names=FALSE)
  assign(paste0("pnad_ocupacao_setores_setores_",i), d);
}

pnad_ocupacao_setores_agregado <-rbindlist(list(pnad_ocupacao_setores_1,
                                        pnad_ocupacao_setores_2,
                                        pnad_ocupacao_setores_3,
                                        pnad_ocupacao_setores_4,
                                        pnad_ocupacao_setores_5,
                                        pnad_ocupacao_setores_6,
                                        pnad_ocupacao_setores_7), use.names=FALSE)

##Exportando Resultados
write.csv(pnad_ocupacao_setores_agregado, file = "pnad_ocupacao_setores_agregado.csv")



