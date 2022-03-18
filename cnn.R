## Construção de uma Rede Neural utilizando o Keras para adquirir novas habilidades
##Carregando a biblioteca Keras
library(keras)
library(tensorflow)
library(plotly)
library(ggplot2)

#################################################################################################
#Construção da CNN 1D para previsão de irradiância solar em diversos horizontes de previsão
model = keras_model_sequential() %>% 
  layer_conv_1d(filters = 32, kernel_size = 3, activation = "relu", input_shape = c(102, 4), padding = "same") %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_conv_1d(filters = 64, kernel_size = 3, activation = "relu", padding = "same") %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_conv_1d(filters = 64, kernel_size = 3, activation = "relu", padding = "same")

model

#Adicionando uma camada densa linear com uma unidade no topo da CNN
model = model %>% 
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1, activation = "linear")

model

#SETUP para a otimização da rede
model %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = c("mae")
)

#Etapa de normalização
train_GHI05 = array_reshape(Data_GHI05_treino, c(22203, 102, 4))
train_GHI05 = train_GHI05/1000
GHI05_train = GHI05_treino/1000


test_GHI05 = array_reshape(Data_GHI05_teste, c(15807, 102, 4))
test_GHI05 = test_GHI05/1000
GHI05_test = GHI05_teste/1000

#Treinamento da rede
model %>% fit(
  Data_GHI05_treino, kt05_GHI_treino,
  epochs = 50, batch_size = 64
)

results = model %>% evaluate(Data_GHI05_teste, kt05_GHI_teste)
results
previsao = predict(model, Data_GHI05_teste)
previsao = previsao * kcs05_GHI_teste
métricas(previsao, GHI05_teste)

#Alocação dos resultados
obs = 1:length(previsao)
previsao05_LBV = previsao
previsao05_sem_LBV = previsao

previsao05_LBV_DNI = previsao
previsao05_sem_LBV_DNI = previsao

write.table(previsao05_LBV, file = "previsao05_LBV.txt" )
write.table(previsao05_sem_LBV, file = "previsao05_sem_LBV.txt" )

write.table(previsao05_LBV_DNI, file = "previsao05_LBV_DNI.txt" )
write.table(previsao05_sem_LBV_DNI, file = "previsao05_sem_LBV_DNI.txt" )

write.table(GHI05_teste, file = "GHI05_teste.txt" )
write.table(DNI05_teste, file = "DNI05_teste.txt" )
