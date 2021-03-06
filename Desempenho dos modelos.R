rm(list=ls())

#Aplica��o dos modelos SVM, KNN, Random Forest, MLM e MLP
#Carregamento das bibliotecas necess�rias
library(caret)
library(caretEnsemble)
library(ggforce)
library(gridExtra)
library(ggplot2)
library(hydroGOF)

#Inicialmente, pr�-processamento dos dados
fix(paredesolar1)
dim(paredesolar1)
sum(is.na(paredesolar1))
paredesolar1=na.omit(paredesolar1)
plot(1:nrow(paredesolar1),paredesolar1$Temperatura.da.agua,xlab = "observa��es",ylab = "Temperaturas da �gua", main = "Temperatura da �gua x observa��es")
range(paredesolar1$Temperatura.da.agua)
names(paredesolar1)
attach(paredesolar1)

#Divis�o do dataset em um conjunto de treinamento e um de valida��o
set.seed(1)
treino=sample(nrow(paredesolar1),0.7*nrow(paredesolar1))

#Etapa utilizada quando trabalhei com wavelets
ywd=cbind(ywd,rad)
ywd=ywd[,-18]
ywd=as.data.frame(ywd)
fix(ywd)
attach(ywd)

paredesolar1_treino=ywd[treino,]
fix(paredesolar1_treino)
paredesolar1_valida��o=ywd[-treino,]
fix(paredesolar1_valida��o)
paredesolar1_treino_normalizado=scale(paredesolar1_treino)
paredesolar1_valida��o_normalizado=scale(paredesolar1_valida��o)
temp_�gua=ywd$rad
temp_�gua_treino=temp_�gua[treino]
temp_�gua_valida��o=temp_�gua[-treino]


#Dataset da agronomia
bcnovo_treino=bcnovo[treino,]
bcnovo_valida��o=bcnovo[-treino,]
sum(is.na(bcnovo_valida��o))
bcnovo_treino_normalizado=scale(bcnovo_treino)
bcnovo_valida��o_normalizado=scale(bcnovo_valida��o)
HP_valida��o=bcnovo$HP[-treino]
HP_treino=bcnovo$HP[treino]

#Dataset da disserta��o
fix(ATRIBUTOS)
DADOS=ATRIBUTOS[,-c(13,16)]
fix(DADOS)
DADOS_treino=DADOS[treino,]
DADOS_valida��o=DADOS[-treino,]
Kt_treino=DADOS$Kt_30[treino]
Kt_valida��o=DADOS$Kt_30[-treino]
H_treino=ATRIBUTOS$RAD_g_30[treino]
H_valida��o=ATRIBUTOS$RAD_g_30[-treino]
H_EXT_valida��o=ATRIBUTOS$RAD_EXT_30[-treino]
H_EXT_treino=ATRIBUTOS$RAD_EXT_30[treino]
H_treino=DADOS$H_10[treino]
H_valida��o=DADOS$H_10[-treino]


#Modelo de persit�ncia A SER RETIFICADO
mp_pred=rep(0,length(temp_�gua_valida��o))
for (i in 1:(nrow(paredesolar1_valida��o)-1)) {
  mp_pred[1]=temp_�gua_valida��o[1]
  mp_pred[i+1]=temp_�gua_valida��o[i]
}
fix(mp_pred)
fix(temp_�gua_valida��o)

#modelo de persist�ncia por meio da m�dia do treinamento
HP_estimado_persist�ncia=mean(bcnovo$HP[treino])
RMSEB=sqrt(mean((HP_estimado_persist�ncia-HP_valida��o)^2))
RMSEB
m�tricas(HP_estimado_persist�ncia,HP_valida��o)

#RMSE do modelo de persist�ncia
temp_agua_persist�ncia=mean(temp_�gua_treino)
RMSEB=sqrt(mean((temp_agua_persist�ncia-temp_�gua_valida��o)^2))
RMSEB

#modelo de persist�ncia para a disserta��o
H_persist�ncia=mean(H_treino)
Kt_baseline=mean(Kt_treino)
RMSEB=sqrt(mean((Kt_baseline-Kt_valida��o)^2))
RMSEB=sqrt(mean((H_persist�ncia-H_valida��o)^2))
m�tricas(H_persist�ncia,H_valida��o)


#Fun��o para calcular diversas m�tricas de erro
m�tricas=function(Y_estimado,Y_real){
  MBE=mean(Y_real-Y_estimado)
  print(MBE)
  MAE=mean(abs(Y_real-Y_estimado))
  print(MAE)
  RMSE=sqrt(mean((Y_real-Y_estimado)^2))
  print(RMSE)
  rRMSE=((RMSE)/mean(Y_real))*100
  print(rRMSE)
  s=((RMSEB-RMSE)/RMSEB)*100
  print(s)
  SSE=sum((Y_real-Y_estimado)^2)
  SSTO=sum((Y_real-mean(Y_real))^2)
  R_squared=1-(SSE/SSTO)
  print(R_squared)
  S=cp(Y_estimado,Y_real)
  print(S)
}

#Configura��o para treinamento por meio de valida��o cruzada 10-Fold
train.control=trainControl(method = "repeatedcv", number = 10, repeats = 10)
names(train.control)
train.control$number
names(paredesolar1)

#Determina��o das f�rmula (dataset parede solar)
n=names(ywd)
f1=as.formula(paste("rad ~", paste(n[!n %in% "rad"], collapse="+")))
f1

#Determina��o das f�mrulas (dataset agronomia)
names(bcnovo)
n=names(bcnovo)
f=as.formula(paste("HP ~", paste(n[!n %in% "HP"], collapse="+")))
f

#Determina��o das f�rmulas (dataset disserta��o)
names(DADOS)
n=names(DADOS)
f=as.formula(paste("Kt_30 ~", paste(n[!n %in% "Kt_30"], collapse="+")))
f

#Treinamento dos modelos 
#SVM com kernel radial(PAREDE SOLAR)
model_SVM=train(f1, data=paredesolar1_treino, method="svmRadial", trControl = train.control, preProcess=c("center", "scale"), metric = "RMSE", verbose = F, tuneLength = 10)
pred_SVM=predict(model_SVM,paredesolar1_valida��o)
m�tricas(pred_SVM,temp_�gua_valida��o)
MLM_m�tricas(pred_SVM,temp_�gua_treino)
names(model_SVM)
SVM=model_SVM$results
fix(SVM)
attach(SVM)
model_SVM$finalModel
model_SVM

#SVM com kernel radial(DISSERTA��O)
model_SVM=train(f, data=DADOS_treino, method="svmRadial", trControl=train.control, preProcess=c("center", "scale"), metric = "RMSE", tuneLength =10, verbose=F)
pred_SVM=predict(model_SVM,DADOS_valida��o)
pred_SVM=H_EXT_valida��o*pred_SVM
m�tricas(pred_SVM,H_valida��o)
MLM_m�tricas(pred_SVM,H_valida��o)
SVM=model_SVM$results
write.table(SVM, "PARAMETROS SVM_sn_10_LOG.txt")
fix(SVM)
model_SVM
model_SVM$finalModel

##################################

#Random Forest (PAREDE SOLAR)
model_RF=train(f1, data=paredesolar1_treino, method="rf", trControl=train.control, ntree=200, preProcess=c("center", "scale"), tuneLength=10)
pred_RF=predict(model_RF,paredesolar1_treino)
m�tricas(pred_RF,temp_�gua_valida��o)
MLM_m�tricas(pred_RF,temp_�gua_treino)
names(model_RF)
modlec=model_RF$results
fix(modlec)
attach(modlec)

#Random Forest (DISSERTA��O)
model_RF=train(f, data=DADOS_treino, method="rf", trControl=train.control, ntree=200, preProcess=c("center", "scale"), tuneLength=10)
pred_RF=predict(model_RF,DADOS_treino)
pred_RF=H_EXT_treino*pred_RF
m�tricas(pred_RF,H_treino)
MLM_m�tricas(pred_RF,H_treino)
names(model_RF)
modlec=model_RF$results
fix(modlec)
attach(modlec)
model_RF$finalModel
write.table(modlec,"PARAMETROS RF_sn_10_LOG.txt")

#################################

#KNN (PAREDE SOLAR)
model_KNN=train(f1, data=paredesolar1_treino, method="knn", trControl=train.control, tuneLength=10, preProcess=c("center","scale"))
pred_KNN=predict(model_KNN,paredesolar1_treino)
m�tricas(pred_KNN,temp_�gua_valida��o)
MLM_m�tricas(pred_KNN,temp_�gua_treino)
names(model_KNN)
KNN=model_KNN$results
fix(KNN)
attach(KNN)

#KNN (AGRONOMIA)
model_KNN=train(f, data=DADOS_treino, method="knn", trControl=train.control, tuneLength=10, preProcess=c("center","scale"))
pred_KNN=predict(model_KNN, DADOS_valida��o)
pred_KNN=H_EXT_valida��o*pred_KNN
m�tricas(pred_KNN,H_valida��o)
MLM_m�tricas(pred_KNN,H_treino)
names(model_KNN)
KNN=model_KNN$results
fix(KNN)
attach(KNN)
write.table(KNN,"PARAMETROS KNN_sn_10_LOG.txt")
model_KNN$finalModel
model_KNN
plot(model_KNN)

###################################
#Bagging (PAREDE SOLAR)
model_BG=train(f1, data=paredesolar1_treino, method="treebag", trControl=train.control, ntree=500, preProcess=c("center", "scale"), tuneLength=10)
pred_BG=predict(model_BG,paredesolar1_treino)
m�tricas(pred_BG,temp_�gua_valida��o)
MLM_m�tricas(pred_BG, temp_�gua_treino)
BG=model_BG$results
fix(BG)
model_BG

######################################
#Bagging (Agronomia)
model_BG=train(f, data=DADOS_treino, method="treebag", trControl=train.control, ntree=500, preProcess=c("center", "scale"), tuneLength=10)
pred_BG=predict(model_BG, DADOS_treino)
pred_BG=H_EXT_treino*pred_BG
m�tricas(pred_BG,H_treino)
MLM_m�tricas(pred_BG, H_treino)
model_BG$finalModel
BAGGING=model_BG$results
fix(BAGGING)

####################################

#Rede MLP usando a biblitoreca RSNSS (parede solar)
model_MLP_RSNSS=train(f1,data = paredesolar1_treino_normalizado,method="mlpML",trControl=train.control,tuneLength=10, preProcess=c("center","scale"))
pred_MLP_RSNSS=predict(model_MLP_RSNSS,paredesolar1_valida��o_normalizado)
fix(pred_MLP_RSNSS)
pred_MLP_RSNSS=(pred_MLP_RSNSS*sd(temp_�gua_valida��o))+mean(temp_�gua_valida��o)
m�tricas(pred_MLP_RSNSS,temp_�gua_valida��o)

#Rede MLP usando a biblitoreca RSNSS (agronomia)
model_MLP_RSNSS=train(f,data = DADOS_treino,method="mlpML",trControl=train.control,tuneLength=10, preProcess=c("center","scale"))
pred_MLP_RSNSS=predict(model_MLP_RSNSS,DADOS_valida��o)
fix(pred_MLP_RSNSS)
pred_MLP_RSNSS=(pred_MLP_RSNSS*sd(HP_valida��o))+mean(HP_valida��o)
m�tricas(pred_MLP_RSNSS,H_valida��o)

#####################################
#Rede Neural com aplica��o do PCA
model_PCA=train(f,data = paredesolar1_treino_normalizado,method="pcaNNet",trControl=train.control,tuneLength=10, preProcess=c("center","scale"))
pred_PCA=predict(model_PCA,paredesolar1_valida��o_normalizado)
fix(pred_PCA)
pred_PCA=(pred_PCA*sd(temp_�gua_valida��o))+mean(temp_�gua_valida��o)
m�tricas(pred_PCA,temp_�gua_valida��o)

#######################################
#Extreme gradient boosting-�rvores (parede solar)
model_XGB=train(f1,data = paredesolar1_treino,method="xgbTree", trControl=train.control, ntree=200, preProcess=c("center","scale"), tuneLength=10)
pred_XGB=predict(model_XGB,paredesolar1_valida��o)
fix(pred_XGB)
m�tricas(pred_XGB,temp_�gua_valida��o)

######################################
#Extreme gradient boosting-�rvores (agronomia)
model_XGB=train(f,data = DADOS_treino,method="xgbTree", trControl=train.control, ntree=200, preProcess=c("center","scale"), tuneLength = 10)
pred_XGB=predict(model_XGB,DADOS_valida��o)
fix(pred_XGB)
m�tricas(pred_XGB,H_valida��o)

#Extreme Machine Learning
library(elmNNRcpp)
Y_MATRIX=matrix(Y_resto,nrow = length(Y_resto), ncol = 1)
paredesolar1_valida��o_MATRIX=as.matrix(paredesolar1_valida��o)
fix(paredesolar1_valida��o_MATRIX)
X_resto=cbind(X_resto,Y_resto)
model_ELM=elm_train(X_resto,Y_MATRIX, nhid=5, actfun = "sig", init_weights = "normal_gaussian", bias = F)
pred_ELM=elm_predict(model_ELM,paredesolar1_valida��o_MATRIX, normalize=T)
fix(pred_ELM)
pred_ELM=(pred_ELM*sd(temp_�gua_valida��o)) + mean(temp_�gua_valida��o)
m�tricas(pred_ELM,temp_�gua_valida��o)

#Rede Neural de base radial
model_RBF=train(f,data = paredesolar1_treino,method="rbfDDA", trControl=train.control, tuneLength=10, preProcess=c("center","scale"))
pred_RBF=predict(model_RBF,paredesolar1_valida��o)
fix(pred_RBF)
pred_RBF=(pred_RBF*sd(temp_�gua_valida��o))+mean(temp_�gua_valida��o)
m�tricas(pred_RBF,temp_�gua_valida��o)

#Visualiza��o gr�fica dos resultados
model_list=list(RF=model_RF, SVM=model_SVM, KNN=model_KNN, BG=model_BG) #MLP=model_MLP_RSNSS, XGB=model_XGB
models=resamples(model_list)
names(model_list)
model_list$SVM
model_list$KNN
model_list$RF
model_list$MLP
model_list$BG
model_list2$XGB

#Constru��o dos gr�ficos
library(ggthemes)
markers=geom_point(size=1.5, shape=6, color="black")
unity=geom_abline(slope=1, intercept = 0, color="red", linetype="dashed",size=1)
bground=theme(axis.title = element_text(face = "bold", size=12), panel.background = element_rect(fill = "lightsteelblue4"),panel.grid = element_line(colour = "grey"),plot.background = element_rect(fill = "grey95"))

#Constru��o dos data frame
data_SVM=as.data.frame(pred_SVM)
data_SVM=cbind(H_valida��o,data_SVM)
data_SVM=cbind(samples,data_SVM)
fix(data_SVM)
write.table(data_SVM,"resultados svm_10.txt")
attach(data_SVM)
P2=ggplot(data = data_SVM, aes(x=`measured values`,y=`estimated values`))+markers+unity
windows()
P2
ggplot(data=data_SVM)+line1+line2+ylab("values")
line3=geom_line(mapping = aes(x=C, y=RMSE),  color="blue")
line4=geom_line(mapping = aes(x=C, y=MAE),  color="red")
line5=geom_line(mapping = aes(x=C, y=Rsquared),  color="black")
ggplot(data = SVM)+ geom_point(mapping = aes(x=C, y=RMSE), shape=6, color="blue") + geom_point(mapping = aes(x=C, y=MAE), shape=1, color="red") + geom_point(mapping = aes(x=C, y=Rsquared), shape=4, color="black")+line3+line4+line5+ylab("values")
##############################

data_RF=as.data.frame(pred_RF)
data_RF=cbind(H_valida��o,data_RF)
data_RF=cbind(samples,data_RF)
fix(data_RF)
attach(data_RF)
write.table(data_RF,"resultados RF_sn_10_kt.txt")
P3=ggplot(data = data_RF, aes(x=`measured values`,y=`estimated values`))+markers+unity
windows()
P3
ggplot(data=data_RF)+line1+line2+ylab("values")
line3=geom_line(mapping = aes(x=mtry, y=RMSE),  color="blue")
line4=geom_line(mapping = aes(x=mtry, y=MAE),  color="red")
line5=geom_line(mapping = aes(x=mtry, y=Rsquared),  color="black")
ggplot(data = modlec)+ geom_point(mapping = aes(x=mtry, y=RMSE), shape=6, color="blue") + geom_point(mapping = aes(x=mtry, y=MAE), shape=1, color="red") + geom_point(mapping = aes(x=mtry, y=Rsquared), shape=4, color="black")+line3+line4+line5+ylab("values")
##############################

data_KNN=as.data.frame(pred_KNN)
data_KNN=cbind(H_valida��o,data_KNN)
fix(data_KNN)
data_KNN=data_KNN[,-2]
attach(data_KNN)
write.table(data_KNN,"resultados KNN_sn_10_kt.txt")
P4=ggplot(data = data_KNN, aes(x=`measured values`,y=`estimated values`))+markers+unity 
windows()
P4
data_KNN=cbind(samples,data_KNN)
fix(data_KNN)
ggplot(data=data_KNN)+line1+line2+ylab("values")
line3=geom_line(mapping = aes(x=k, y=RMSE),  color="blue")
line4=geom_line(mapping = aes(x=k, y=MAE),  color="red")
line5=geom_line(mapping = aes(x=k, y=Rsquared),  color="black")
ggplot(data = KNN)+ geom_point(mapping = aes(x=k, y=RMSE), shape=6, color="blue") + geom_point(mapping = aes(x=k, y=MAE), shape=1, color="red") + geom_point(mapping = aes(x=k, y=Rsquared), shape=4, color="black")+line3+line4+line5+ylab("values")
##############################

data_BG=as.data.frame(pred_BG)
data_BG=cbind(H_valida��o,data_BG)
fix(data_BG)
attach(data_BG)
write.table(data_BG,"resultados BG_sn_10_LOG.txt")
P5=ggplot(data = data_BG, aes(x=`measured values`,y=`estimated values`))+markers+unity
windows()
P5
data_BG=cbind(samples,data_BG)
fix(data_BG)
attach(data_BG)
ggplot(data=data_BG)+line1+line2+ylab("values")

##############################

data_MLP=as.data.frame(pred_MLP_RSNSS)
data_MLP=cbind(HP_valida��o,data_MLP)
fix(data_MLP)
attach(data_MLP)
P5=ggplot(data = data_MLP, aes(x=`Valores reais`,y=`Valores estimados`))+markers+unity+bground 
windows()
P5
##############################

data_XGB=as.data.frame(pred_XGB)
data_XGB=cbind(HP_valida��o,data_XGB)
fix(data_XGB)
attach(data_XGB)
P6=ggplot(data = data_XGB, aes(x=`Valores reais`,y=`Valores estimados`))+markers+unity+bground 
windows()
P6
##############################

P1=ggplot(,aes(x=, y=pred_RF))+markers+unity+bground 
P2=ggplot(,aes(x=temp_�gua_valida��o, y=pred_SVM))+markers+unity+bground
P3=ggplot(,aes(x=temp_�gua_valida��o, y=pred_KNN))+markers+unity+bground
P4=ggplot(,aes(x=temp_�gua_valida��o, y=pred_MLP))+markers+unity+bground
P5=ggplot(,aes(x=temp_�gua_valida��o, y=pred_MLP_RSNSS))+markers+unity+bground
P6=ggplot(,aes(x=temp_�gua_valida��o, y=pred_XGB))+markers+unity+bground
windows()
grid.arrange(P1,P2,P3,P4,nrow=2)
windows()
grid.arrange(P5,P6, nrow=1)
windows()
scales=list(x=list(relation="free"),y=list(relation="free"))
dotplot(models,scales=scales, layout=c(3,1))
windows()
scales2=list(x=list(relation="free"),y=list(relation="free"))
dotplot(models2,scales=scales2, layout=c(3,1))
##############################

#Gr�fico boxplot
fix(Modelos.x.Rmse)
attach(Modelos.x.Rmse)
ggplot(data = Modelos.x.Rmse, aes(x=Models, y=RMSE, colour=Models))+geom_boxplot()

#Constru��o de um mapa de calor
fix(bcnovo)
sum(is.na(bcnovo))
bcnovo=na.omit(bcnovo)
library(plotly)
package_version(plotly)
p=plot_ly(x=names(bcnovo), y=names(bcnovo),z=cor(bcnovo), type = "heatmap")
p

#Constru��o de outros mapas de calor
p1=plot_ly(x=names(bcnovo), y=names(bcnovo),z=cor(bcnovo), colors = "Greys", type = "heatmap")
p1

p2=plot_ly(x=names(bcnovo), y=names(bcnovo),z=cor(bcnovo), colors =colorRamp(c ("purple","green")), type = "heatmap")
p2
Sys.setenv("plotly_username"="fpmarinho90")
Sys.setenv("plotly_api_key"="QpBH8YnNamwouItR2YNI")
chart_link=api_create(p2, filename = "heatmap")
chart_link


vals=unique(scales::rescale(c(cor(bcnovo))))
o=order(vals, decreasing = F)
cols=scales::col_numeric("Blues", domain = "NULL")(vals)
colz=setNames(data.frame(vals[0], cols[0]), NULL)
p3=plot_ly(x=names(bcnovo), y=names(bcnovo), z=cor(bcnovo), colorscale = colz, type = "heatmap")
#Criar um link compartilh�vel para o gr�fico
chart_link=api_create(p, filename = "heatmap-simple")
chart_link
