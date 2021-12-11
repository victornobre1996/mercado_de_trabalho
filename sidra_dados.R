# Este script tem como objetivo mexer com os dados do sidra

# carregando os pacotes

library(tidyverse)
library(sidrar)
library(dplyr)


# baixando a base 

sidra <- get_sidra(x = 1846,
                   variable = 585,
                   period = c("201904","202001", "202002", "202003", "202004", "202101", "202102"),
                   classific = "c11255",
                   header = TRUE)

# manipulando a base 

sidra_manipulado <- sidra %>% 
  select(`Trimestre (Código)`,`Setores e subsetores (Código)`, Valor) %>% 
  rename(trimestre = `Trimestre (Código)`,
         setores = `Setores e subsetores (Código)`) %>% 
  filter(setores %in% c("90687","90691","90693",
                        "90695","90694","90697",
                        "90698", "90699","90700",
                        "90702","90701","90703", "90705"))


sidra_manipulado <- sidra_manipulado %>% 
  
  














