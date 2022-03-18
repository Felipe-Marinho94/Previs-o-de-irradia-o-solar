rm(list = ls())
#Pré-processamento dos dados - Dados FolsomData.mat
#Autor: Felipe Pinto Marinho
#Data: 06/09/2021

#Carregando algumas bibliotecas
library(keras)
library(tensorflow)
library(rTensor)
library(R.matlab)

#Carregando o dataset FolsomData
Folsom = readMat("C:/Users/Felipe Pinto/Downloads/FolsomData.mat")
names(Folsom)
Folsom_features = Folsom$features
Folsom_target = Folsom$target

#Pré-processamento dos dados
#Observação dos dados
#Features GHI e DNI
Folsom_features_GHI = Folsom_features[[1]]
Folsom_features_DNI = Folsom_features[[2]]
dim(Folsom_features_GHI)
dim(Folsom_features_DNI)

#Preditores L, B, e V
Folsom_features_GHI_B = Folsom_features_GHI[[1]]
Folsom_features_GHI_L = Folsom_features_GHI[[2]]
Folsom_features_GHI_V = Folsom_features_GHI[[3]]

Folsom_features_DNI_B = Folsom_features_DNI[[1]]
Folsom_features_DNI_L = Folsom_features_DNI[[2]]
Folsom_features_DNI_V = Folsom_features_DNI[[3]]

###############################################################
#Separação treino e testes para os features
#Preditores L, B e V para GHI e para DNI
#Treino
GHI_B_treino = Folsom_features_GHI_B[[1]]
GHI_L_treino = Folsom_features_GHI_L[[1]]
GHI_V_treino = Folsom_features_GHI_V[[1]]

#Teste
GHI_B_teste = Folsom_features_GHI_B[[3]]
GHI_L_teste = Folsom_features_GHI_L[[3]]
GHI_V_teste = Folsom_features_GHI_V[[3]]

#################################################################
#Preditores L, B e V para DNI
#Treino
DNI_B_treino = Folsom_features_DNI_B[[1]]
DNI_L_treino = Folsom_features_DNI_L[[1]]
DNI_V_treino = Folsom_features_DNI_V[[1]]

#Teste
DNI_B_teste = Folsom_features_DNI_B[[3]]
DNI_L_teste = Folsom_features_DNI_L[[3]]
DNI_V_teste = Folsom_features_DNI_V[[3]]

##################################################################
#Preditores para os canais R, G, B
Folsom_features_R = Folsom_features[[3]]
Folsom_features_G = Folsom_features[[4]]
Folsom_features_B = Folsom_features[[5]]
Folsom_features_Rate = Folsom_features[[6]]

#Canal Red
R_media = Folsom_features_R[[1]]
R_std = Folsom_features_R[[2]]
R_entropy = Folsom_features_R[[3]]

#Separação treino e teste
#treino
R_media_treino = R_media[[1]]
R_std_treino = R_std[[1]]
R_entropy_treino = R_entropy[[1]]

#Teste
R_media_teste = R_media[[3]]
R_std_teste = R_std[[3]]
R_entropy_teste = R_entropy[[3]]

##################################################################
#Canal Green
G_media = Folsom_features_G[[1]]
G_std = Folsom_features_G[[2]]
G_entropy = Folsom_features_G[[3]]

#Separação treino e teste
#Treino
G_media_treino = G_media[[1]]
G_std_treino = G_std[[1]]
G_entropy_treino = G_entropy[[1]]

#Teste
G_media_teste = G_media[[3]]
G_std_teste = G_std[[3]]
G_entropy_teste = G_entropy[[3]]

####################################################################
#Canal Blue
B_media = Folsom_features_B[[1]]
B_std = Folsom_features_B[[2]]
B_entropy = Folsom_features_B[[3]]

#Separação treino e teste
#Treino
B_media_treino = B_media[[1]]
B_std_treino = B_std[[1]]
B_entropy_treino = B_entropy[[1]]

#Teste
B_media_teste = B_media[[3]]
B_std_teste = B_std[[3]]
B_entropy_teste = B_entropy[[3]]

#####################################################################
#Razão Red to Blue
Rate_media = Folsom_features_Rate[[1]]
Rate_std = Folsom_features_Rate[[2]]
Rate_entropy = Folsom_features_Rate[[3]]

#Separação treino e teste
#Treino
Rate_media_treino = Rate_media[[1]]
Rate_std_treino = Rate_std[[1]]
Rate_entropy_treino = Rate_entropy[[1]]

#Teste
Rate_media_teste = Rate_media[[3]]
Rate_std_teste = Rate_std[[3]]
Rate_entropy_teste = Rate_entropy[[3]]

######################################################################
#Targets para todos os horizontes
FH05 = Folsom_target[[1]]
FH10 = Folsom_target[[2]]
FH15 = Folsom_target[[3]]
FH20 = Folsom_target[[4]]
FH25 = Folsom_target[[5]]
FH30 = Folsom_target[[6]]

######################################################################
#Targets para GHI
Targets_GHI05 = FH05[[2]]
Targets_GHI10 = FH10[[2]]
Targets_GHI15 = FH15[[2]]
Targets_GHI20 = FH20[[2]]
Targets_GHI25 = FH25[[2]]
Targets_GHI30 = FH30[[2]]

######################################################################
#Targets para DNI
Targets_DNI05 = FH05[[3]]
Targets_DNI10 = FH10[[3]]
Targets_DNI15 = FH15[[3]]
Targets_DNI20 = FH20[[3]]
Targets_DNI25 = FH25[[3]]
Targets_DNI30 = FH30[[3]]

######################################################################
#Estabelecendo como saídas os valores de irradiância medidos
#GHI treino
GHI05_treino = Targets_GHI05[[1]][[3]] * 1000
GHI10_treino = Targets_GHI10[[1]][[3]] * 1000
GHI15_treino = Targets_GHI15[[1]][[3]] * 1000
GHI20_treino = Targets_GHI20[[1]][[3]] * 1000
GHI25_treino = Targets_GHI25[[1]][[3]] * 1000
GHI30_treino = Targets_GHI30[[1]][[3]] * 1000

#kt GHI treino
kt05_GHI_treino = Targets_GHI05[[1]][[2]]
kt10_GHI_treino = Targets_GHI10[[1]][[2]]
kt15_GHI_treino = Targets_GHI15[[1]][[2]]
kt20_GHI_treino = Targets_GHI20[[1]][[2]]
kt25_GHI_treino = Targets_GHI25[[1]][[2]]
kt30_GHI_treino = Targets_GHI30[[1]][[2]]

#Clear Sky Irradiance GHI treino
kcs05_GHI_treino = Targets_GHI05[[1]][[1]] * 1000
kcs10_GHI_treino = Targets_GHI10[[1]][[1]] * 1000
kcs15_GHI_treino = Targets_GHI15[[1]][[1]] * 1000
kcs20_GHI_treino = Targets_GHI20[[1]][[1]] * 1000 
kcs25_GHI_treino = Targets_GHI25[[1]][[1]] * 1000
kcs30_GHI_treino = Targets_GHI30[[1]][[1]] * 1000

#GHI teste
GHI05_teste = Targets_GHI05[[3]][[3]] * 1000
GHI10_teste = Targets_GHI10[[3]][[3]] * 1000
GHI15_teste = Targets_GHI15[[3]][[3]] * 1000
GHI20_teste = Targets_GHI20[[3]][[3]] * 1000
GHI25_teste = Targets_GHI25[[3]][[3]] * 1000
GHI30_teste = Targets_GHI30[[3]][[3]] * 1000

#kt GHI teste
kt05_GHI_teste = Targets_GHI05[[3]][[2]]
kt10_GHI_teste = Targets_GHI10[[3]][[2]]
kt15_GHI_teste = Targets_GHI15[[3]][[2]]
kt20_GHI_teste = Targets_GHI20[[3]][[2]]
kt25_GHI_teste = Targets_GHI25[[3]][[2]]
kt30_GHI_teste = Targets_GHI30[[3]][[2]]

#Clear Sky Irradiance GHI teste
kcs05_GHI_teste = Targets_GHI05[[3]][[1]] * 1000
kcs10_GHI_teste = Targets_GHI10[[3]][[1]] * 1000
kcs15_GHI_teste = Targets_GHI15[[3]][[1]] * 1000
kcs20_GHI_teste = Targets_GHI20[[3]][[1]] * 1000 
kcs25_GHI_teste = Targets_GHI25[[3]][[1]] * 1000
kcs30_GHI_teste = Targets_GHI30[[3]][[1]] * 1000

##########################################################
#DNI treino
DNI05_treino = Targets_DNI05[[1]][[3]] * 1000
DNI10_treino = Targets_DNI10[[1]][[3]] * 1000
DNI15_treino = Targets_DNI15[[1]][[3]] * 1000
DNI20_treino = Targets_DNI20[[1]][[3]] * 1000
DNI25_treino = Targets_DNI25[[1]][[3]] * 1000
DNI30_treino = Targets_DNI30[[1]][[3]] * 1000

#kt DNI treino
kt05_DNI_treino = Targets_DNI05[[1]][[2]]
kt10_DNI_treino = Targets_DNI10[[1]][[2]]
kt15_DNI_treino = Targets_DNI15[[1]][[2]]
kt20_DNI_treino = Targets_DNI20[[1]][[2]]
kt25_DNI_treino = Targets_DNI25[[1]][[2]]
kt30_DNI_treino = Targets_DNI30[[1]][[2]]

#Clear Sky Irradiance DNI treino
kcs05_DNI_treino = Targets_DNI05[[1]][[1]] * 1000
kcs10_DNI_treino = Targets_DNI10[[1]][[1]] * 1000
kcs15_DNI_treino = Targets_DNI15[[1]][[1]] * 1000
kcs20_DNI_treino = Targets_DNI20[[1]][[1]] * 1000
kcs25_DNI_treino = Targets_DNI25[[1]][[1]] * 1000
kcs30_DNI_treino = Targets_DNI30[[1]][[1]] * 1000

#DNI teste
DNI05_teste = Targets_DNI05[[3]][[3]] * 1000
DNI10_teste = Targets_DNI10[[3]][[3]] * 1000
DNI15_teste = Targets_DNI15[[3]][[3]] * 1000
DNI20_teste = Targets_DNI20[[3]][[3]] * 1000
DNI25_teste = Targets_DNI25[[3]][[3]] * 1000
DNI30_teste = Targets_DNI30[[3]][[3]] * 1000

#kt DNI teste
kt05_DNI_teste = Targets_DNI05[[3]][[2]]
kt10_DNI_teste = Targets_DNI10[[3]][[2]]
kt15_DNI_teste = Targets_DNI15[[3]][[2]]
kt20_DNI_teste = Targets_DNI20[[3]][[2]]
kt25_DNI_teste = Targets_DNI25[[3]][[2]]
kt30_DNI_teste = Targets_DNI30[[3]][[2]]

#Clear Sky Irradiance DNI treino
kcs05_DNI_teste = Targets_DNI05[[3]][[1]] * 1000
kcs10_DNI_teste = Targets_DNI10[[3]][[1]] * 1000
kcs15_DNI_teste = Targets_DNI15[[3]][[1]] * 1000
kcs20_DNI_teste = Targets_DNI20[[3]][[1]] * 1000
kcs25_DNI_teste = Targets_DNI25[[3]][[1]] * 1000
kcs30_DNI_teste = Targets_DNI30[[3]][[1]] * 1000

############################################################
#Construção do Array de treino
#Para GHI no horizonte de 5 minutos
Data_GHI_R = cbind(GHI_B_treino, GHI_L_treino, GHI_V_treino, R_media_treino, R_std_treino, R_entropy_treino)
treino_R = as.data.frame(Data_GHI_R)
dim(treino_R)

Data_GHI_G = cbind(GHI_B_treino, GHI_L_treino, GHI_V_treino, G_media_treino, G_std_treino, G_entropy_treino)
treino_G = as.data.frame(Data_GHI_G)
dim(treino_G)

Data_GHI_B = cbind(GHI_B_treino, GHI_L_treino, GHI_V_treino, B_media_treino, B_std_treino, B_entropy_treino)
treino_B = as.data.frame(Data_GHI_B)
dim(treino_B)

Data_GHI_Rate = cbind(GHI_B_treino, GHI_L_treino, GHI_V_treino, Rate_media_treino, Rate_std_treino, Rate_entropy_treino)
treino_Rate = as.data.frame(Data_GHI_Rate)
dim(treino_Rate)

Data_GHI_treino = array(0, dim = c(22203, 102, 4))
Data_GHI_treino[, , 1] = Data_GHI_R
Data_GHI_treino[, , 2] = Data_GHI_G
Data_GHI_treino[, , 3] = Data_GHI_B
Data_GHI_treino[, , 4] = Data_GHI_Rate
Data_GHI_treino = Data_GHI05_treino

##################################################################
#Construção do Array de treino
#Para DNI no horizonte de 5 minutos
Data_DNI_R = cbind(DNI_B_treino, DNI_L_treino, DNI_V_treino, R_media_treino, R_std_treino, R_entropy_treino)
treino_R = as.data.frame(Data_DNI_R)
dim(treino_R)

Data_DNI_G = cbind(DNI_B_treino, DNI_L_treino, DNI_V_treino, G_media_treino, G_std_treino, G_entropy_treino)
treino_G = as.data.frame(Data_DNI_G)
dim(treino_G)

Data_DNI_B = cbind(DNI_B_treino, DNI_L_treino, DNI_V_treino, B_media_treino, B_std_treino, B_entropy_treino)
treino_B = as.data.frame(Data_DNI_B)
dim(treino_B)

Data_DNI_Rate = cbind(DNI_B_treino, DNI_L_treino, DNI_V_treino, Rate_media_treino, Rate_std_treino, Rate_entropy_treino)
treino_Rate = as.data.frame(Data_DNI_Rate)
dim(treino_Rate)

Data_DNI_treino = array(0, dim = c(22203, 102, 4))
Data_DNI_treino[, , 1] = Data_DNI_R
Data_DNI_treino[, , 2] = Data_DNI_G
Data_DNI_treino[, , 3] = Data_DNI_B
Data_DNI_treino[, , 4] = Data_DNI_Rate
dim(Data_DNI_treino)

##################################################################
#Construção do Array de teste
#Para GHI no horizonte de 5 minutos
Data_GHI_R_teste = cbind(GHI_B_teste, GHI_L_teste, GHI_V_teste, R_media_teste, R_std_teste, R_entropy_teste)
teste_R = as.data.frame(Data_GHI_R_teste)
dim(teste_R)

Data_GHI_G_teste = cbind(GHI_B_teste, GHI_L_teste, GHI_V_teste, G_media_teste, G_std_teste, G_entropy_teste)
teste_G = as.data.frame(Data_GHI_G_teste)
dim(teste_G)

Data_GHI_B_teste = cbind(GHI_B_teste, GHI_L_teste, GHI_V_teste, B_media_teste, B_std_teste, B_entropy_teste)
teste_B = as.data.frame(Data_GHI_B_teste)
dim(teste_B)

Data_GHI_Rate_teste = cbind(GHI_B_teste, GHI_L_teste, GHI_V_teste, Rate_media_teste, Rate_std_teste, Rate_entropy_teste)
teste_Rate = as.data.frame(Data_GHI_Rate_teste)
dim(teste_Rate)

Data_GHI_teste = array(0, dim = c(15807, 102, 4))
Data_GHI_teste[, , 1] = Data_GHI_R_teste
Data_GHI_teste[, , 2] = Data_GHI_G_teste
Data_GHI_teste[, , 3] = Data_GHI_B_teste
Data_GHI_teste[, , 4] = Data_GHI_Rate_teste
Data_GHI_teste = Data_GHI05_teste

############################################################
#Construção do Array de teste
#Para DNI no horizonte de 5 minutos
Data_DNI_R_teste = cbind(DNI_B_teste, DNI_L_teste, DNI_V_teste, R_media_teste, R_std_teste, R_entropy_teste)
teste_R = as.data.frame(Data_DNI_R_teste)
dim(teste_R)

Data_DNI_G_teste = cbind(DNI_B_teste, DNI_L_teste, DNI_V_teste, G_media_teste, G_std_teste, G_entropy_teste)
teste_G = as.data.frame(Data_DNI_G_teste)
dim(teste_G)

Data_DNI_B_teste = cbind(DNI_B_teste, DNI_L_teste, DNI_V_teste, B_media_teste, B_std_teste, B_entropy_teste)
teste_B = as.data.frame(Data_DNI_B_teste)
dim(teste_B)

Data_DNI_Rate_teste = cbind(DNI_B_teste, DNI_L_teste, DNI_V_teste, Rate_media_teste, Rate_std_teste, Rate_entropy_teste)
teste_Rate = as.data.frame(Data_DNI_Rate_teste)
dim(teste_Rate)

Data_DNI_teste = array(0, dim = c(15807, 102, 4))
Data_DNI_teste[, , 1] = Data_DNI_R_teste
Data_DNI_teste[, , 2] = Data_DNI_G_teste
Data_DNI_teste[, , 3] = Data_DNI_B_teste
Data_DNI_teste[, , 4] = Data_DNI_Rate_teste
Data_DNI_teste = Data_DNI05_teste

############################################################
#Construção do Array de treino
#Para GHI no horizonte de 5 minutos
#Desconsiderando a informação dos preditores L, G, B
Data_GHI_R_ATEMP = cbind(R_media_treino, R_std_treino, R_entropy_treino)
treino_R_ATEMP = as.data.frame(Data_GHI_R_ATEMP)
dim(treino_R_ATEMP)

Data_GHI_G_ATEMP = cbind(G_media_treino, G_std_treino, G_entropy_treino)
treino_G_ATEMP = as.data.frame(Data_GHI_G_ATEMP)
dim(treino_G_ATEMP)

Data_GHI_B_ATEMP = cbind(B_media_treino, B_std_treino, B_entropy_treino)
treino_B_ATEMP = as.data.frame(Data_GHI_B_ATEMP)
dim(treino_B_ATEMP)

Data_GHI_Rate_ATEMP = cbind(Rate_media_treino, Rate_std_treino, Rate_entropy_treino)
treino_Rate_ATEMP = as.data.frame(Data_GHI_Rate_ATEMP)
dim(treino_Rate_ATEMP)

Data_GHI_treino_ATEMP = array(0, dim = c(22203, 30, 4))
Data_GHI_treino_ATEMP[, , 1] = Data_GHI_R_ATEMP
Data_GHI_treino_ATEMP[, , 2] = Data_GHI_G_ATEMP
Data_GHI_treino_ATEMP[, , 3] = Data_GHI_B_ATEMP
Data_GHI_treino_ATEMP[, , 4] = Data_GHI_Rate_ATEMP
Data_GHI_treino_ATEMP = Data_GHI05_treino_ATEMP

##################################################################
#Construção do Array de teste
#Para GHI no horizonte de 5 minutos
#Desconsiderando a informação dos preditores L, G, B
Data_GHI_R_teste_ATEMP = cbind(R_media_teste, R_std_teste, R_entropy_teste)
teste_R_ATEMP = as.data.frame(Data_GHI_R_teste_ATEMP)
dim(teste_R_ATEMP)

Data_GHI_G_teste_ATEMP = cbind(G_media_teste, G_std_teste, G_entropy_teste)
teste_G_ATEMP = as.data.frame(Data_GHI_G_teste_ATEMP)
dim(teste_G_ATEMP)

Data_GHI_B_teste_ATEMP = cbind(B_media_teste, B_std_teste, B_entropy_teste)
teste_B_ATEMP = as.data.frame(Data_GHI_B_teste_ATEMP)
dim(teste_B_ATEMP)

Data_GHI_Rate_teste_ATEMP = cbind(Rate_media_teste, Rate_std_teste, Rate_entropy_teste)
teste_Rate_ATEMP = as.data.frame(Data_GHI_Rate_teste_ATEMP)
dim(teste_Rate_ATEMP)

Data_GHI_teste_ATEMP = array(0, dim = c(15807, 30, 4))
Data_GHI_teste_ATEMP[, , 1] = Data_GHI_R_teste_ATEMP
Data_GHI_teste_ATEMP[, , 2] = Data_GHI_G_teste_ATEMP
Data_GHI_teste_ATEMP[, , 3] = Data_GHI_B_teste_ATEMP
Data_GHI_teste_ATEMP[, , 4] = Data_GHI_Rate_teste_ATEMP
Data_GHI_teste_ATEMP = Data_GHI05_teste_ATEMP

###############################################################################
#Construção de conjuntos de dados para uso na LSTM
dados_treino_GHI = cbind(GHI_B_treino, GHI_L_treino, GHI_V_treino, R_media_treino, R_std_treino, R_entropy_treino, G_media_treino, G_std_treino, G_entropy_treino, B_media_treino, B_std_treino, B_entropy_treino, Rate_media_treino, Rate_std_treino, Rate_entropy_treino)
dados_teste_GHI = cbind(GHI_B_teste, GHI_L_teste, GHI_V_teste, R_media_teste, R_std_teste, R_entropy_teste, G_media_teste, G_std_teste, G_entropy_teste, B_media_teste, B_std_teste, B_entropy_teste, Rate_media_teste, Rate_std_teste, Rate_entropy_teste)
dim(dados_teste_GHI)

dados_treino_atemp = cbind(R_media_treino, R_std_treino, R_entropy_treino, G_media_treino, G_std_treino, G_entropy_treino, B_media_treino, B_std_treino, B_entropy_treino, Rate_media_treino, Rate_std_treino, Rate_entropy_treino)
dados_teste_atemp = cbind(R_media_teste, R_std_teste, R_entropy_teste, G_media_teste, G_std_teste, G_entropy_teste, B_media_teste, B_std_teste, B_entropy_teste, Rate_media_teste, Rate_std_teste, Rate_entropy_teste)
dim(dados_treino_atemp)

dados_treino_DNI = cbind(DNI_B_treino, DNI_L_treino, DNI_V_treino, R_media_treino, R_std_treino, R_entropy_treino, G_media_treino, G_std_treino, G_entropy_treino, B_media_treino, B_std_treino, B_entropy_treino, Rate_media_treino, Rate_std_treino, Rate_entropy_treino)
dados_teste_DNI = cbind(DNI_B_teste, DNI_L_teste, DNI_V_teste, R_media_teste, R_std_teste, R_entropy_teste, G_media_teste, G_std_teste, G_entropy_teste, B_media_teste, B_std_teste, B_entropy_teste, Rate_media_teste, Rate_std_teste, Rate_entropy_teste)

#Construção do conjunto de dados para aplicação do TPR
Data_GHI_treino_TPR = array(0, dim = c(4, 102, 22203))
Data_GHI_treino_TPR[1, ,] = t(Data_GHI_R)
Data_GHI_treino_TPR[2, ,] = t(Data_GHI_G)
Data_GHI_treino_TPR[3, ,] = t(Data_GHI_B)
Data_GHI_treino_TPR[4, ,] = t(Data_GHI_Rate)
dim(Data_GHI_treino_TPR)

Data_GHI_teste_TPR = array(0, dim = c(4, 102, 15807))
Data_GHI_teste_TPR[1, ,] = t(Data_GHI_R_teste)
Data_GHI_teste_TPR[2, ,] = t(Data_GHI_G_teste)
Data_GHI_teste_TPR[3, ,] = t(Data_GHI_B_teste)
Data_GHI_teste_TPR[4, ,] = t(Data_GHI_Rate_teste)
dim(Data_GHI_teste_TPR)

Data_GHI_treino_TPR[, , 1]
Data_GHI_teste_TPR[, , 1]


#Exportando as entradas
#GHI
write.table(Data_GHI_treino, file = "Data_GHI_treino.txt")
write.table(Data_GHI_teste, file = "Data_GHI_teste.txt")


#DNI
write.table(Data_DNI_treino, file = "Data_DNI_treino.txt")
write.table(Data_DNI_teste, file = "Data_DNI_teste.txt")

#ATEMP
write.table(Data_GHI_treino_ATEMP, file = "Data_ATEMP_treino.txt")
write.table(Data_GHI_teste_ATEMP, file = "Data_ATEMP_teste.txt")
dim(Data_GHI_treino_ATEMP)


#Exportando as saídas
#Exportando as entradas
#GHI05
write.table(GHI05_treino, file = "GHI05_treino.txt")
write.table(GHI05_teste, file = "GHI05_teste.txt")
write.table(kt05_GHI_treino, file = "kt05_GHI_treino.txt")
write.table(kt05_GHI_teste, file = "kt05_GHI_teste.txt")
write.table(kcs05_GHI_treino, file = "kcs05_GHI_treino.txt")
write.table(kcs05_GHI_teste, file = "kcs05_GHI_teste.txt")

#DNI
write.table(Data_DNI_treino, file = "Data_DNI_treino.txt")
write.table(Data_DNI_teste, file = "Data_DNI_teste.txt")