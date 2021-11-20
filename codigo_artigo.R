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

## RENDIMENTO MEDIO POR SETOR ##

pnad_rendimento_medio_setor_2019 <- svyby(formula =~VD4019, by = VD4010, design = pnad_2019, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_setor_2020_1 <- svyby(formula =~VD4019, by = VD4010, design = pnad_2020_1, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_setor_2020_2 <- svyby(formula =~VD4019, by = VD4010, design = pnad_2020_2, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_setor_2020_3 <- svyby(formula =~VD4019, by = VD4010, design = pnad_2020_3, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_setor_2020_4 <- svyby(formula =~VD4019, by = VD4010, design = pnad_2020_4, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_setor_2021_1 <- svyby(formula =~VD4019, by = VD4010, design = pnad_2021_1, FUN = svymean, na.rm = TRUE )
pnad_rendimento_medio_setor_2021_2 <- svyby(formula =~VD4019, by = VD4010, design = pnad_2021_2, FUN = svymean, na.rm = TRUE )

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


#---- transformando rowname para variavel e separando ----

pnad_rendimento_agregado_setor <- pnad_rendimento_agregado_setor %>% 
  separate( 
    col = "interaction(VD3004, VD4010)", 
    into = c("instrucao", "setor"),
    sep = "\\.")

# organizar dataframe

pnad_rendimento_agregado_setor <- pnad_rendimento_agregado_setor %>% select(trimestre,instrucao, setor, VD4019)

pnad_rendimento_agregado_setor <- pnad_rendimento_agregado_setor %>% rename(rendimento_medio_nominal = VD4019)

pnad_rendimento_agregado_setor$rendimento_medio_nominal <- round(pnad_rendimento_agregado_setor$rendimento_medio_nominal, digits = 2)

## agregando os dados 

pnad_rendimento_agregado_setor <- pnad_rendimento_agregado_setor %>% 
  mutate(setor = case_when(setor == "Educação, saúde humana e serviços sociais" ~ 
                             "Administração pública, defesa e seguridade social ",
                           TRUE ~ setor))




# ---- exportando a base no R ---- 
write.csv(pnad_rendimento_agregado_setor, file = "pnad_rendimento_agregado_setor.csv")


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

## agregando os dados 

pnad_rendimento_agregado <- pnad_rendimento_agregado %>% 
  mutate(setor = case_when(setor == "Educação, saúde humana e serviços sociais" ~ 
                             "Administração pública, defesa e seguridade social ",
                           TRUE ~ setor))




# ---- exportando a base no R ---- 
write.csv(pnad_rendimento_agregado, file = "Pnad_rendimento_agregado.csv")

## ##





