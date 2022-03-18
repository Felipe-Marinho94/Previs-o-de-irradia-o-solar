# Implementação de uma rede com camadas
# CNN-1D e LSTM

#Carregando algumas bibliotecas importantes
library(tensorflow)
library(keras)
library(plotly)
library(ggplot2)

#Convertendo os dataframes de teste de treino e de teste em matrizes de floating
treino = data.matrix(Data_GHI_treino)
teste = data.matrix(Data_GHI_teste)

#Normalização dos dados
media = apply(treino, 2, mean)
std = apply(treino, 2, sd)

data_treino = scale(treino, center = media, scale = std)
data_teste = scale(teste, center = media, scale = std)

##################################################################################
#Desenvolvimento da arquitetura da CNN-LSTM
model_CNN_LSTM = keras_model_sequential() %>%
  layer_conv_1d(filters = 32, kernel_size = 3, activation = "relu", input_shape = c(30, 4), padding = "same") %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_conv_1d(filters = 32, kernel_size = 3, activation = "relu") %>%
  layer_lstm(units = 32, dropout = 0.1, recurrent_dropout = 0.5) %>%
  layer_dense(units = 1)

model_CNN_LSTM

#Setup da otimização
model_CNN_LSTM %>% compile(optimizer = "adam",
                           loss = "mse",
                           metrics = list("mae"))

#Treinamento do modelo
model_CNN_LSTM %>% fit(
  Data_GHI_treino_ATEMP, kt30_DNI_treino,
  epochs = 50, batch_size = 64, shuffle = F
)

#Avaliação do modelo
results = model_CNN_LSTM %>% evaluate(Data_GHI_teste_ATEMP, kt30_DNI_teste)
results
previsao = predict(model_CNN_LSTM, Data_GHI_teste_ATEMP)
previsao = previsao * kcs30_DNI_teste
métricas(previsao, DNI30_teste)

#Alocação dos resultados
previsao30_LBV_CNN_LSTM = previsao
previsao30_sem_LBV_CNN_LSTM = previsao

previsao30_LBV_DNI_CNN_LSTM = previsao
previsao30_sem_LBV_DNI_CNN_LSTM = previsao

write.table(previsao30_LBV_CNN_LSTM, file = "previsao30_LBV_CNN_LSTM.txt" )
write.table(previsao30_sem_LBV_CNN_LSTM, file = "previsao30_sem_LBV_CNN_LSTM.txt" )

write.table(previsao30_LBV_DNI_CNN_LSTM, file = "previsao30_LBV_DNI_CNN_LSTM.txt" )
write.table(previsao30_sem_LBV_DNI_CNN_LSTM, file = "previsao30_sem_LBV_DNI_CNN_LSTM.txt" )

