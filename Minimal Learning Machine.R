rm(list=ls())
#minimal learning machine
#Etapa de treinamento
#Importe os atributos de treinamento, Importe as saídas de treinamento
#Estabeleça um valor de k
#Pré-processamento
paredesolar1=paredesolar.TiO2.75PPM
fix(paredesolar1)
dim(paredesolar1)
sum(is.na(ywd))
X=ywd
Y=rad

sum(is.na(bcnovo))
bcnovo=na.omit(bcnovo)
fix(bcnovo)
X=bcnovo[,-44]
fix(X)
dim(X)
Y=bcnovo$HP
fix(Y)
length(Y)
is.numeric(X[1,3])
X_normalizado=scale(X)
Y_normalizado=scale(Y)
fix(X_normalizado)
fix(Y_normalizado)

#Procedimento com o conjunto de dados utilizando os presitores californianos e o Kt
fix(ATRIBUTOS.30min)
sum(is.na(ATRIBUTOS.30min))
ATRIBUTOS.30min=na.omit(ATRIBUTOS.30min)
ATRIBUTOS=ATRIBUTOS.30min[,-c(1,2,3,4,5,6,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)]
fix(ATRIBUTOS)

RAD_EXT_30=ATRIBUTOS$RAD_EXT_30
RAD_30=ATRIBUTOS$RAD_g_30
fix(RAD_EXT_30)

ATRIBUTOS_f=ATRIBUTOS[,-c(13,14,16)]
fix(ATRIBUTOS_f)

X=ATRIBUTOS_f
Y=ATRIBUTOS$Kt_30
fix(X)
fix(Y)
X_normalizado=scale(X)
Y_normalizado=scale(Y)

#Sessão de pré-processamento para o conjunto de dados da dissertação
fix(LDR_PARAMETRIZADO)

# Para o caso de previsão de radiação global
DADOS=LDR_PARAMETRIZADO[,-c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)]

#Para o caso de previsão de radiação direta
DADOS=LDR_PARAMETRIZADO[,-c(7,8,9,10,11,12,13,23,24,25,26,27,28)]

#Continuando 
DADOS=DADOS[,-c(7,8,9,10,11)]
fix(DADOS)
sum(is.na(DADOS))
DADOS=na.omit(DADOS)

#Inicialmente a previsão será realizada para um horizonte de previsão de
#30 minutos a frente
DADOS=DADOS[,-c(6,7)]
fix(DADOS)
X=DADOS[,-6]
fix(X)
Y=DADOS$H_10
X_normalizado=scale(X)
Y_normalizado=scale(Y)

#Divisão do conjunto de dados em um resto e validação
set.seed(1)
treino=sample(nrow(X),0.7*nrow(X))
X_resto=X_normalizado[treino,]
fix(X_resto)
dim(X_resto)
Y_resto=Y_normalizado[treino]
fix(Y_resto)

#Aplicação da validação cruzada para a obtenção do k ótimo
library(caret)
dados=cbind(X_resto,Y_resto)
dim(dados)
fix(dados)
index=createFolds(Y_resto, k=10, list = T, returnTrain = F)
names(index)
Rmse=0
erros=rep(0,99)
for (k in 2:100) {
  teste=index$Fold01
  X_treino=X_resto[-teste,]
  Y_treino=Y_resto[-teste]
  Rmse=MLM(X_treino,Y_treino,k)
  vetor1=Rmse
  
  teste=index$Fold02
  X_treino=X_resto[-teste,]
  Y_treino=Y_resto[-teste]
  Rmse=MLM(X_treino,Y_treino,k)
  vetor2=Rmse
  
  teste=index$Fold03
  X_treino=X_resto[-teste,]
  Y_treino=Y_resto[-teste]
  Rmse=MLM(X_treino,Y_treino,k)
  vetor3=Rmse
  
  teste=index$Fold04
  X_treino=X_resto[-teste,]
  Y_treino=Y_resto[-teste]
  Rmse=MLM(X_treino,Y_treino,k)
  vetor4=Rmse
  
  teste=index$Fold05
  X_treino=X_resto[-teste,]
  Y_treino=Y_resto[-teste]
  Rmse=MLM(X_treino,Y_treino,k)
  vetor5=Rmse
  
  teste=index$Fold06
  X_treino=X_resto[-teste,]
  Y_treino=Y_resto[-teste]
  Rmse=MLM(X_treino,Y_treino,k)
  vetor6=Rmse
  
  teste=index$Fold07
  X_treino=X_resto[-teste,]
  Y_treino=Y_resto[-teste]
  Rmse=MLM(X_treino,Y_treino,k)
  vetor7=Rmse
  
  teste=index$Fold08
  X_treino=X_resto[-teste,]
  Y_treino=Y_resto[-teste]
  Rmse=MLM(X_treino,Y_treino,k)
  vetor8=Rmse
  
  teste=index$Fold09
  X_treino=X_resto[-teste,]
  Y_treino=Y_resto[-teste]
  Rmse=MLM(X_treino,Y_treino,k)
  vetor9=Rmse
  
  teste=index$Fold10
  X_treino=X_resto[-teste,]
  Y_treino=Y_resto[-teste]
  Rmse=MLM(X_treino,Y_treino,k)
  vetor10=Rmse
  erros[k]=mean(vetor1,vetor2,vetor3,vetor4,vetor5,vetor6,vetor7,vetor8,vetor9,vetor10)
}
fix(erros)
erros=erros[-1]
erros[which.min(erros)]
erros[20]
library(ggplot2)
plot(1:99,erros, xlab = "valor de k", ylab = "RMSE",type = "b", main = "RMSE estimado por validação cruzada 10-fold")
points(which.min(erros), erros[which.min(erros)], col="red", cex=2, pch=20)
which.min(erros)
write.table(erros,"erros_sn_10_LOG.txt")

#Criação da função minimal learning machine
MLM=function(X_treino,Y_treino,k){
  ref=sample(nrow(X_treino),k)
  R=X_treino[ref,]
  TO=Y_treino[ref]
  Dx=matrix(1:9,nrow = nrow(X_treino), ncol=k)
  Dy=matrix(1:9,nrow =nrow(X_treino), ncol = k )
  
  #Cálculo das matrizes de distâncias
  for (i in 1:nrow(X_treino)) {
    for (j in 1:k) {
      Dx[i,j]=sqrt(sum((X_treino[i,]-R[j,])^2))
      Dy[i,j]=sqrt(sum((Y_treino[i]-TO[j])^2))
    }
    
  }
  
  #Cálculo da matriz dos coeficientes
  library(MASS)
  A=t(Dx)%*%Dx
  B=t(Dx)%*%Dy
  C=ginv(A)
  Bhat=C%*%B
  
  #Segunda etapa: Teste
  X_teste=X_normalizado[treino,]
  d=matrix(1:10,nrow = nrow(X_teste),ncol = k)
  for (i in 1:nrow(X_teste)) {
    for (j in 1:k) {
      d[i,j]=sqrt(sum((X_teste[i,]-R[j,])^2))
    }
  }
  library(minpack.lm)
  #Determinação das estimativas para as distâncias
  delta=d%*%Bhat
  
  #Determinação das saídas
  y_inicial=(45-mean(Y))/sd(Y)
  y=y_inicial
  saída_estimada=rep(0,nrow(delta))
  #vamos lá
  for (i in 1:nrow(delta)) {
    vetor=delta[i,]
    residuos=function(y){
      residuos=(y-TO)^2-(vetor)^2
      residuos
    }
    lm=nls.lm(par= y_inicial,lower = NULL, upper = NULL, fn = residuos)
    saída_estimada[i]=lm$par
  }
  saída_estimada=(saída_estimada)*sd(Y)+mean(Y)
  
  #Determinação do MSE e RMSE
  mean((Y[teste]-saída_estimada)^2)
  Rmse=sqrt(mean((Y[teste]-saída_estimada)^2))
  return(saída_estimada)
}

teste=index$Fold01
dim(X_resto)
X_treino=X_resto[-teste,]
fix(X_treino)
Y_treino=Y_resto[-teste]
fix(Y_treino)
pqp=MLM(X_treino,Y_treino,10)
pqp

################
#Etapa de validação
X_treino=X_normalizado[treino,]
Y_treino=Y_normalizado[treino]
saída_estimada=MLM(X_treino,Y_treino,95)
RAD_estimada=saída_estimada*H_EXT_treino
fix(saída_estimada)
fix(RAD_estimada)
métricas(RAD_estimada,H_treino)
MLM_métricas(saída_estimada, H_treino)

#################
#teste
teste=function(y){
  teste=t(y-TO)*(y-TO)-(delta[1,])^2
  teste
}
LM=nls.lm(par = y_inicial,lower = NULL, upper = NULL, fn=teste)
names(LM)
(LM$par*sd(Y))+ mean(Y)

chute=c(1,2,3)
m=c(1,2,3)
teste=function(y){
  teste=(y-chute)^2-(m)^2
  teste
}
teste(1)

#Construção de gráficos para visualização dos resultados
library(ggthemes)
library(caretEnsemble)
library(ggforce)
library(gridExtra)
data1=as.data.frame(H_validação)
data1=cbind(data1,RAD_estimada)
fix(data1)
attach(data1)
write.table(data1,"resultados mlm_sn_10.txt")
markers=geom_point(size=1.5, shape=6, color="black")
unity=geom_abline(slope=1, intercept = 0, color="red", linetype="dashed",size=1)
bground=theme(axis.title = element_text(face = "bold", size=12), panel.background = element_rect(fill = "lightsteelblue4"),panel.grid = element_line(colour = "grey"),plot.background = element_rect(fill = "grey95"))
P1=ggplot(data = data1, aes(x=`measured values`,y=`estimated values`))+markers+unity
P1=ggplot(data = data1, aes(x=`valores medidos`,y=`valores estimados`))+markers+unity
windows()
P1

samples=1:length(H_validação)
length(samples)
samples=as.data.frame(1:length(temp_água_validação))
is.vector(samples)
fix(samples)
attach(samples)
data1=cbind(samples,data1)
fix(data1)
line1=geom_line(mapping = aes(x=samples,y=`valores medidos`), color="green", size=1)
line2=geom_line(mapping = aes(x=samples,y=`valores estimados`), color="black",size=1)
ggplot(data = data1) + geom_point(mapping = aes(x=samples,y=`measured values`), color="violet", shape=1) + geom_point(mapping = aes(x=samples,y=`estimated values`), color="black", shape=6)

ggplot(data=data1)+line1+line2+ylab("values")
line3=geom_line(mapping = aes(x=k, y=RMSE),  color="blue")
line4=geom_line(mapping = aes(x=k, y=MAE),  color="red")
line5=geom_line(mapping = aes(x=k, y=R_squared),  color="black")
attach(metricas.mlm)
names(metricas.mlm)
ggplot(data = metricas.mlm)+ geom_point(mapping = aes(x=k, y=RMSE), shape=6, color="blue") + geom_point(mapping = aes(x=k, y=MAE), shape=1, color="red") + geom_point(mapping = aes(x=k, y=R_squared), shape=4, color="black")+line3+line4+line5+ylab("values")

#Criação de uma função para calcular RMSE, MAE e R²
MLM_métricas=function(Y_estimado,Y_real){
  RMSE=sqrt(mean((Y_real-Y_estimado)^2))
  print(RMSE)
  
  MAE=mean(abs(Y_real-Y_estimado))
  print(MAE)
  
  SSE=sum((Y_real-Y_estimado)^2)
  SSTO=sum((Y_real-mean(Y_real))^2)
  R_squared=1-(SSE/SSTO)
  print(R_squared)
}

#Aplicação da função acima definida
teste=index$Fold09
X_treino=X_normalizado[-teste,]
Y_treino=Y_normalizado[-teste]
saída_estimada=MLM(X_treino,Y_treino,69)
Y_resto_treino=Y[-teste]
fix(saída_estimada)
length(saída_estimada)
fix(Y_resto_treino)
length(Y_resto_treino)
MLM_métricas(saída_estimada,Y_resto_treino)
erros[65]


#Gráficos dos hiperparâmetros
library(plotly)
fix(PARAMETROS.SVM)
fix(modlec)
fix(KNN)
fix(erros)
pontos=rep(2:100)
ERROS=as.data.frame(erros)
ERROS=cbind(pontos,ERROS)
fix(ERROS)
attach(ERROS)
attach(KNN)
attach(modlec)
attach(PARAMETROS.SVM)
plot_ly(PARAMETROS.SVM, x=C, y=RMSE, name='RMSE', type = 'scatter',mode='lines+markers') %>% add_trace(y=MAE, name='MAE', type='scatter', mode='lines+markers') %>% layout(title='Hyperparameter search in SVR', xaxis=list(title='C'), yaxis=list(title='values'))
plot_ly(modlec, x=mtry, y=RMSE, name='RMSE', type = 'scatter',mode='lines+markers') %>% add_trace(y=MAE, name='MAE', type='scatter', mode='lines+markers') %>% layout(title='Hyperparameter search in Random Forest', xaxis=list(title='m'), yaxis=list(title='values'))
plot_ly(KNN, x=KNN$k, y=RMSE, name='RMSE', type = 'scatter',mode='lines+markers') %>% add_trace(y=MAE, name='MAE', type='scatter', mode='lines+markers') %>% layout(title='Hyperparameter search in KNN', xaxis=list(title='K'), yaxis=list(title='values') )
plot_ly(ERROS, x=pontos, y=erros, name='RMSE', type = 'scatter',mode='lines+markers', line=list(color='black')) %>% layout(title='Busca por hiperparâmetro no MLM', xaxis=list(title='K'), yaxis=list(title='RMSE') )

#Gráficos dos resultados
attach(data_SVM)
plot_ly(data_SVM, x=samples, y=`estimated values`, name='estimated values', type = 'scatter', mode='line') %>% add_trace(y=`measured values` , name='measured values', type = 'scatter', mode='line') %>% layout(title='Results for SVR', xaxis=list(title='sample'), yaxis=list(title='values'))
