rm(list = ls())


# Script utilizado para puxar setores por formal e informal 


library(tidyverse)
library(PNADcIBGE)
library(survey)
library(data.table)

# baixando as bases


var_select <- c("VD4002", "V2009", "VD4019", "VD3004", "VD4010", 
                "VD4008", "VD4009", "VD4011" ,"VD4012","V4032")


# baixando os datasets do IBGE

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



# agregando as linhas de formais 

pnad_formais_agregado <- rbind(`pnad_formais_2019/4T`,
                                   `pnad_formais_2020/1T`,
                                   `pnad_formais_2020/2T`,
                                   `pnad_formais_2020/3T`,
                                   `pnad_formais_2020/4T`,
                                   `pnad_formais_2021/1T`,
                                   `pnad_formais_2021/2T`,
                                   `pnad_formais_2021/3T`)


# a partir do dataset agregado, excluir demais datasets
      
rm(`pnad_formais_2019/4T`,
         `pnad_formais_2020/1T`,
         `pnad_formais_2020/2T`,
         `pnad_formais_2020/3T`,
         `pnad_formais_2020/4T`,
         `pnad_formais_2021/1T`,
         `pnad_formais_2021/2T`,
         `pnad_formais_2021/3T`)


# adicionando a coluna de contribuintes e nao contribuintes bem como formais e informais

pnad_formais_agregado_1 <- pnad_formais_agregado %>% 
    mutate(tipo_formal = case_when(contribuinte == "Não contribuinte" ~ "informal",
                                 contribuinte == "Contribuinte" ~ "formal",
                             TRUE ~ contribuinte)) %>%
  rename(total_ocupados = `summary(na.omit(interaction(droplevels(p$variables$VD4010), droplevels(p$variables$VD4009), droplevels(p$variables$VD4012), drop = T)))`) %>% 
na.omit(pnad_formais_agregado_1) %>% 
  select(trimestre,setor, tipo_formal,total_ocupados)


### somando as categorias

teste <- pnad_formais_agregado_1 %>% 
  mutate(setor = case_when(setor == "Educação, saúde humana e serviços sociais" ~ "Administração pública, defesa e seguridade social ",
                           setor == "Alojamento e alimentação" ~ "Outros Serviços",
                           setor == "Serviços domésticos" ~ "Outros Serviços",
                           TRUE ~ setor))



# exportando planilha

write.csv(pnad_formais_agregado_1, file = "pnad_formais.csv")
  
 







