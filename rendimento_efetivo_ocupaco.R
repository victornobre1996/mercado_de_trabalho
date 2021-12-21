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
pnad_2021_3 <- get_pnadc(year = 2021, quarter = 3, vars = var_select, deflator = F)

mylist <- list(pnad_2019, pnad_2020_1, pnad_2020_2, pnad_2020_3,
               pnad_2020_4, pnad_2020_1, pnad_2021_2, pnad_2021_3)
mylist_1 <-list("2019/4T", "2020/1T", "2020/2T", "2020/3T",
                "2020/4T", "2021/1T", "2021/2T", "2021/3T")

for (i in seq_along(mylist)) {
  p <- mylist[[i]]
  a <- svyby(formula =~(VD4020_real = VD4020*Efetivo), by = ~VD4009, 
             design = p, FUN = svymean, na.rm = TRUE )
  a <- a %>% select(-c("Empregador", "Conta-própria"))  #qq coisa tira essa
    
  a1 <- svyby(formula =~(VD4020_real = VD4020*Efetivo), by = ~interaction(VD4009 == "Empregador" &
                                                                   VD4012=="Contribuinte"), 
              design = p, FUN = svymean, na.rm = TRUE )
  
  a2 <- svyby(formula =~(VD4020_real = VD4020*Efetivo), by = ~interaction(VD4009 == "Empregador" &
                                                                   VD4012=="Não contribuinte"), 
              design = p, FUN = svymean, na.rm = TRUE )
  
  a3 <- svyby(formula =~(VD4020_real = VD4020*Efetivo), by = ~interaction(VD4009 == "Conta-própria" &
                                                                   VD4012=="Contribuinte"),
              design = p, FUN = svymean, na.rm = TRUE )
  
  a4 <- svyby(formula =~(VD4020_real = VD4020*Efetivo), by = ~interaction(VD4009 == "Conta-própria" &
                                                                   VD4012=="Não contribuinte"),
              design = p, FUN = svymean, na.rm = TRUE )
  
              
  d <- rbindlist(list(a,a1,a2,a3,a4), use.names=FALSE) %>% 
    mutate(trimestre = mylist_1[[i]])
  
  assign(paste0("pnad_rendiento_ocupados",i), d)            
  
}

##falta resto do código

