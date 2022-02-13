library(tidyverse)
library(ggplot2)

# criando dataset

dados <- data.frame(setores = c("comercio",
                                "transporte",
                                "outros servicos",
                                "icafi",
                                "apdss"),
                    produtividade = c("-0,006",
                                      "0,0092",
                                      "-0,0016",
                                      "-0,0031",
                                      "-0,0035"),
                    OC = c("0,0056",
                           "-0,0008",
                           "0,0149",
                           "0,0123",
                           "0,0006"))


# plotando gráfico de correlacao 

dados %>% 
  ggplot(aes(x=produtividade, y=OC)) +
  geom_point(aes(color = setores)) +
  geom_text(aes(label=setores), nudge_x = 0.25, nudge_y = 0.25) + 
  labs(title = "Correlacao entre produtividade e pessoal ocupado",
       subtitle = "por setor",
       caption = "Fonte: FGV") +
  theme(legend.position = "none")
