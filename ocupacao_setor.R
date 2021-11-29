##Posição na Ocupação - Por setor ##

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

##Modificações nos Tipos de Ocupação##
a_1 <- a$variables$VD4009[10]
a_2 <- a$variables$VD4009[59]
o_cp <- recode(a_1, "Conta-própria" = "Conta-própria que contribuí para PS")
o_ncp <- recode(a_1, "Conta-própria" = "Conta-própria que não contribuí para PS")
o_emp <- recode(a_1, "Conta-própria" = "Empregador que contribuí para PS")
o_nemp <- recode(a_1, "Conta-própria" = "Empregador que não contribuí para PS")

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
  
  # *** Discriminando contribuição para Previdência Social (PS) ***
  a$variables$VD4019[a$variables$VD4019 == a_1 & a$variables$VD4012 == "Contribuinte" ] <- as.factor(o_cp)
  a$variables$VD4019[a$variables$VD4019 == a_1 & a$variables$VD4012 != "Contribuinte" ] <- as.factor(o_ncp)
  a$variables$VD4019[a$variables$VD4019 == a_2 & a$variables$VD4012 == "Contribuinte" ] <- as.factor(o_emp)
  a$variables$VD4019[a$variables$VD4019 == a_2 & a$variables$VD4012 != "Contribuinte" ] <- as.factor(o_nemp)
  
  assign(paste0("pnad_",mylist_1[[W]]),a);
}
rm(r,a,e,f,aea,h,g,var_select)

view(a$variables$VD4012)
#Ocupação - Total da Economia #

mylist_1 <-list("2019/4T", "2020/1T", "2020/2T", "2020/3T",
                "2020/4T", "2021/1T", "2021/2T")

for (i in seq_along(mylist)) {
  p <- mylist[[i]]
  a<-as.data.frame(summary(
    na.omit(p$variables$VD4009))) %>% 
    mutate(trimestre = mylist_1[[i]])
  
  a <- a %>% mutate(row.names(a)) 
  
  b <-as.data.frame(summary(na.omit(
    interaction((p$variables$VD4009),
                (p$variables$VD4012),
                drop = T)))) %>% 
    mutate(trimestre = mylist_1[[i]])
  
  b <- b %>% mutate(row.names(b))
  
  b<-(b[c("Conta-própria.Contribuinte",
          "Conta-própria.Não contribuinte",
          "Empregador.Contribuinte",
          "Empregador.Não contribuinte"),])
  
  d <- rbindlist(list(a,b), use.names=FALSE)
  assign(paste0("pnad_ocupacao_",i), d)
  rm(a,b,d);
}

pnad_ocupacao_agregado <-rbindlist(list(pnad_ocupacao_1,
                                        pnad_ocupacao_2,
                                        pnad_ocupacao_3,
                                        pnad_ocupacao_4,
                                        pnad_ocupacao_5,
                                        pnad_ocupacao_6,
                                        pnad_ocupacao_7), use.names=FALSE)

##Exportando Resultados
write.csv(pnad_ocupacao_agregado, file = "pnad_ocupacao_agregado.csv")



