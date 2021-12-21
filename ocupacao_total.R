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
pnad_2021_3 <- get_pnadc(year = 2021, quarter = 3, vars = var_select)

#Ocupação - Total da Economia #

mylist <- list(pnad_2019, pnad_2020_1, pnad_2020_2, pnad_2020_3,
               pnad_2020_4, pnad_2020_1, pnad_2021_2, pnad_2021_3)
mylist_1 <-list("2019/4T", "2020/1T", "2020/2T", "2020/3T",
                "2020/4T", "2021/1T", "2021/2T", "2021/3T")

for (i in seq_along(mylist)) {
  p <- mylist[[i]]
  a<-as.data.frame(summary(
    na.omit(p$variables$VD4009))) %>% 
    mutate(trimestre = mylist_1[[i]])
  a <- a %>% select(-c("Empregador", "Conta-própria"))
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
                                        pnad_ocupacao_7,
                                        pnad_ocupacao_8), use.names=FALSE)


pnad_ocupacao_agregado$Numero_de_Ocupados <- pnad_ocupacao_agregado$`summary(na.omit(p$variables$VD4009))`
pnad_ocupacao_agregado$Tipo_de_ocupacao <-pnad_ocupacao_agregado$`row.names(a)`

pnad_ocupacao_agregado <- pnad_ocupacao_agregado %>% select(trimestre, Tipo_de_ocupacao ,
                                                                            Numero_de_Ocupados)


##Exportando Resultados
write.csv(pnad_ocupacao_agregado, file = "pnad_ocupacao_agregado.csv")



