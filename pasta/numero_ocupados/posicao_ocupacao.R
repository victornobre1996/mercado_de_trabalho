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

var_select <- c("VD4002", "V2009", "VD4019", "VD3004", "VD4010", "VD4009", "VD4012", "V4032")

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
                  (p$variables$VD4032),
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

# para os setores e posicoes de ocupacao 

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



# numero de ocupados por setor e posicao na ocupacao 


pnad_2019_setor_ocupacao <- svyby(formula =~as.integer(VD4002 == "Pessoas ocupadas"),
                        by = ~interaction(VD4009,VD4010), design = subset(pnad_2019,V2009 >=14), FUN=svytotal, keep.names=FALSE, na.rm=TRUE)

pnad_2020_1_setor_ocupacao <- svyby(formula =~as.integer(VD4002 == "Pessoas ocupadas"),
                                  by = ~interaction(VD4009,VD4010), design = subset(pnad_2020_1,V2009 >=14), FUN=svytotal, keep.names=FALSE, na.rm=TRUE)

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


# agregando os diferentes trimestres por ocupacao


pnad_2019_setor_ocupacao <- pnad_2019_setor_ocupacao %>% mutate(trimestre = "2019/4T")
pnad_2020_1_setor_ocupacao <- pnad_2020_1_setor_ocupacao %>% mutate(trimestre = "2020/1T")
pnad_2020_2_setor_ocupacao <- pnad_2020_2_setor_ocupacao %>% mutate(trimestre = "2020/2T")
pnad_2020_3_setor_ocupacao <- pnad_2020_3_setor_ocupacao %>% mutate(trimestre = "2020/3T")
pnad_2020_4_setor_ocupacao <- pnad_2020_4_setor_ocupacao %>% mutate(trimestre = "2020/4T")
pnad_2021_1_setor_ocupacao <- pnad_2021_1_setor_ocupacao %>% mutate(trimestre = "2021/1T")
pnad_2021_2_setor_ocupacao <- pnad_2021_2_setor_ocupacao %>% mutate(trimestre = "2021/2T")



pnad_setor_ocupacao_agregado <- rbindlist(list(pnad_2019_setor_ocupacao,
                                       pnad_2020_1_setor_ocupacao,
                                       pnad_2020_2_setor_ocupacao,
                                       pnad_2020_3_setor_ocupacao,
                                       pnad_2020_4_setor_ocupacao,
                                       pnad_2021_1_setor_ocupacao,
                                       pnad_2021_2_setor_ocupacao))


# separando as colunas de setor e posicao na ocupacao 

pnad_setor_ocupacao_agregado <- pnad_setor_ocupacao_agregado %>% 
  separate( 
    col = "interaction(VD4009, VD4010)", 
    into = c("posicao_ocupacao", "setor"),
    sep = "\\.")


View(pnad_setor_ocupacao_agregado)


###

pnad_setor_ocupacao_agregado <- pnad_setor_ocupacao_agregado %>% 
  select(trimestre, posicao_ocupacao, setor, `as.integer(VD4002 == "Pessoas ocupadas")`) %>% 
  rename(total_ocupados = `as.integer(VD4002 == "Pessoas ocupadas")`)


pnad_setor_ocupacao_agregado$total_ocupados <- round(pnad_setor_ocupacao_agregado$total_ocupados, digits = 0)

View(pnad_setor_ocupacao_agregado)

### exportar os dados 

write.csv(pnad_setor_ocupacao_agregado,file = "pnad_setor_ocupacao_agregado.csv")





