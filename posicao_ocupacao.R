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

var_select <- c("VD4019", "VD3004", "VD4010", "VD4009", "VD4012")

pnad_2019 <- get_pnadc(year = 2019, quarter = 4, vars = var_select)
pnad_2020_1 <- get_pnadc(year = 2020, quarter = 1, vars = var_select)
pnad_2020_2 <- get_pnadc(year = 2020, quarter = 2, vars = var_select)
pnad_2020_3 <- get_pnadc(year = 2020, quarter = 3, vars = var_select)
pnad_2020_4 <- get_pnadc(year = 2020, quarter = 4, vars = var_select)
pnad_2021_1 <- get_pnadc(year = 2021, quarter = 1, vars = var_select)
pnad_2021_2 <- get_pnadc(year = 2021, quarter = 2, vars = var_select)

#Ocupação - Total da Economia #

mylist <- list(pnad_2019, pnad_2020_1, pnad_2020_2, pnad_2020_3,
               pnad_2020_4, pnad_2021_1, pnad_2021_2)
mylist_1 <-list("2019/4T", "2020/1T", "2020/2T", "2020/3T",
                "2020/4T", "2021/1T", "2021/2T")

for (h in seq_along(mylist_1)) {
  for (i in seq_along(mylist)) {
    p <- mylist[[i]]
    a<-as.data.frame(summary(
      na.omit(p$variables$VD4009))) %>% 
      mutate(trimestre = mylist_1[[h]])
    
    a <- a %>% mutate(row.names(a)) 
    
    b <-as.data.frame(summary(na.omit(
      interaction((p$variables$VD4009),
                  (p$variables$VD4012),
                  drop = T)))) %>% 
      mutate(trimestre = mylist_1[[h]])
    
    b <- b %>% mutate(row.names(b))
    
    b<-(b[c("Conta-própria.Contribuinte",
            "Conta-própria.Não contribuinte",
            "Empregador.Contribuinte",
            "Empregador.Não contribuinte"),])
    
    d <- rbindlist(list(a,b), use.names=FALSE)
    assign(paste0("pnad_ocupacao_",h), d)
    rm(a,b,d);
  }
}

pnad_ocupacao_agregado <-rbindlist(list(pnad_ocupacao_1,
                                        pnad_ocupacao_2,
                                        pnad_ocupacao_3,
                                        pnad_ocupacao_4,
                                        pnad_ocupacao_5,
                                        pnad_ocupacao_6,
                                        pnad_ocupacao_7), use.names=FALSE)




