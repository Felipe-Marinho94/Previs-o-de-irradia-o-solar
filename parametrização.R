rm(list = ls())

#Programa para realizar a parametrização entre os dados obtidos 
#pelo raspberry pi e aqueles coletados pelo MODAS

#Pré-processamento dos dados
fix(`070619_2MODAS.10.min`)
LDR=`070619_2LDR`
VALORES_MODAS=`070619_2MODAS.10.min`[,-c(1,2,3,4,5,6)]
fix(VALORES_MODAS)
TEMPO_MODAS=`070619_2MODAS.10.min`[,c(1,2,3,4,5,6)]
fix(TEMPO_MODAS)
sum(is.na(LDR))
sum(is.na(VALORES_MODAS))
sum(is.na(TEMPO_MODAS))
VALORES_MODAS=na.omit(VALORES_MODAS)
TEMPO_MODAS=na.omit(TEMPO_MODAS)
fix(LDR)
fix(MODAS)

#vamos começar a brincadeira
d=rep(0,nrow(VALORES_MODAS))
RAD_GLOBAL=rep(0,nrow(LDR))
RAD_DIFUSA=rep(0,nrow(LDR))

#Utilizando o kt
VALORES=matrix(0,nrow = nrow(LDR), ncol = ncol(VALORES_MODAS))
VALORES=as.data.frame(VALORES)
fix(VALORES)

for (i in 1:nrow(LDR)) {
  for (j in 1:nrow(VALORES_MODAS)) {
    DATA_LDR=ISOdatetime(LDR[i,1],LDR[i,2],LDR[i,3],LDR[i,4],LDR[i,5],LDR[i,6])
    DATA_MODAS=ISOdatetime(TEMPO_MODAS[j,1],TEMPO_MODAS[j,2],TEMPO_MODAS[j,3],TEMPO_MODAS[j,4],TEMPO_MODAS[j,5],TEMPO_MODAS[j,6])
    d[j]=abs(as.numeric(DATA_LDR)-as.numeric(DATA_MODAS))
  }
#  RAD_GLOBAL[i]=MODAS$H[which.min(d)]
#  RAD_DIFUSA[i]=MODAS$DIFUSA[which.min(d)]
  
  VALORES[i,]=VALORES_MODAS[which.min(d),]
}
RAD=cbind(RAD_GLOBAL,RAD_DIFUSA)
fix(RAD)
fix(VALORES)
write.table(VALORES, "VALORES070619_2.txt")
