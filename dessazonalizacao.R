
nova_base <-Cópia_de_nova_tabela_mod
data <- nova_base[,1]
nova_base <- nova_base[,-1]
labels <-colnames(nova_base)
b <-as.list(colnames(nova_base))

dados_agregados <- list()

for (i in seq_along(b)) {
  a <- nova_base[,i]
  a <- ts(a, start = c(2012,1), end = c(2021,3) , frequency = 4)
  fit = StructTS(a, type = "BSM")
  decomp = cbind(a, fitted(fit))
  colnames(decomp) = c("data","level","slope", "seasonal")
  a <- decomp[,"data"] - decomp[,"seasonal"]
  assign(paste0("dados_",i), a) -> dados_agregados[[i]]
}






dados_agregados <- as.data.frame(dados_agregados)
colnames(dados_agregados) <- labels

write_excel_csv2(dados_agregados, file = "VA_Ocupacao_dessazo.csv")







