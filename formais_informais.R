rm(list = ls())


# Script utilizado para puxar setores por formal e informal 


library(tidyverse)
library(PNADcIBGE)
library(survey)
library(data.table)

# baixando as bases

var_select <- c("VD4002", "V2009", "VD4019", "VD3004", "VD4010", 
                "VD4008", "VD4009", "VD4011" ,"VD4012","V4032")


pnad_2019 <- get_pnadc(year = 2019, quarter = 4, vars = var_select)
pnad_2020_1 <- get_pnadc(year = 2020, quarter = 1, vars = var_select)
pnad_2020_2 <- get_pnadc(year = 2020, quarter = 2, vars = var_select)
pnad_2020_3 <- get_pnadc(year = 2020, quarter = 3, vars = var_select)
pnad_2020_4 <- get_pnadc(year = 2020, quarter = 4, vars = var_select)
pnad_2021_1 <- get_pnadc(year = 2021, quarter = 1, vars = var_select)
pnad_2021_2 <- get_pnadc(year = 2021, quarter = 2, vars = var_select)
pnad_2021_3 <- get_pnadc(year = 2021, quarter = 3, vars = var_select)


## Ocupação - Categorias PNAD

my_list <- list(pnad_2019,pnad_2020_1,pnad_2020_2,pnad_2020_3,pnad_2020_4,
                pnad_2021_1,pnad_2021_2,pnad_2021_3)

my_list_1 <- list("2019/4T","2020/1T","2020/2T", "2020/3T",
                  "2020/4T", "2021/1T", "2021/2T", "2021/3T")


for (i in seq_along(my_list)) {
  p <- my_list[[i]]
  a <- as.data.frame(summary(na.omit(
  interaction(
     droplevels(p$variables$VD4010),
             droplevels(p$variables$VD4009),
              droplevels(p$variables$VD4012),
              drop = T)))) %>% 
    mutate(trimestre = my_list_1[[i]]) %>% 
  rownames_to_column(var = "model") %>% 
    separate( 
      col = "model", 
      into = c("setor" , "ocupacao", "contribuinte"),
      sep = "\\.") 
  assign(paste0("pnad_formais_",my_list_1[[i]]), a)  

}



# agregando as linhas de acordo com as colunas 

pnad_formais_agregado <- rbind(`pnad_formais_2019/4T`,
                                   `pnad_formais_2020/1T`,
                                   `pnad_formais_2020/2T`,
                                   `pnad_formais_2020/3T`,
                                   `pnad_formais_2020/4T`,
                                   `pnad_formais_2021/1T`,
                                   `pnad_formais_2021/2T`,
                                   `pnad_formais_2021/3T`)


# removendo as demais linhas
      
rm(`pnad_formais_2019/4T`,
         `pnad_formais_2020/1T`,
         `pnad_formais_2020/2T`,
         `pnad_formais_2020/3T`,
         `pnad_formais_2020/4T`,
         `pnad_formais_2021/1T`,
         `pnad_formais_2021/2T`,
         `pnad_formais_2021/3T`)


# fazendo manipulacoes

pnad_formais_agregado_1 <- pnad_formais_agregado %>% 
  rename(total_ocupados = `summary(na.omit(interaction(droplevels(p$variables$VD4010), 
                                                       droplevels(p$variables$VD4009),
                                                       droplevels(p$variables$VD4012), drop = T)))`) %>% 
  mutate(tipo_formal = case_when(contribuinte == "Não contribuinte" ~ "informal",
                                 contribuinte == "Contribuinte" ~ "formal",
                             TRUE ~ contribuinte)) %>%
  select(trimestre,setor,ocupacao, contribuinte, tipo_formal, total_ocupados) %>% 
  na.omit(pnad_formais_agregado_1) 



rm(pnad_formais_2)

pnad_formais_2 <- pnad_formais_agregado_1 %>% 
  mutate(setor = case_when(setor == "Educação, saúde humana e serviços sociais" ~ "Administração pública, defesa e seguridade social",
                           TRUE ~ setor)) %>% 
  group_by(setor) %>% 
  summarise(total_ocupados = sum(total_ocupados))

pnad_formais_2











write.csv(pnad_formais_agregado_1, file = "pnad_formais.csv")
  









