#Implementação LSTM usando KERAS
#Carregando algumas bibliotecas
library(keras)
library(tensorflow)
library(ggplot2)
library(plotly)

#Convertendo os dataframes de teste de treino e de teste em matrizes de floating
treino = data.matrix(dados_treino_DNI)
teste = data.matrix(dados_teste_DNI)

#Normalização dos dados
media = apply(treino, 2, mean)
std = apply(treino, 2, sd)

data_treino = scale(treino, center = media, scale = std)
data_teste = scale(teste, center = media, scale = std)


##################################################################################
#Arquitetura
model_ls = keras_model_sequential() %>%   
  layer_lstm(units=32, input_shape=c(5, 192), activation="relu", dropout = 0.1, recurrent_dropout = 0.1, return_sequences = T) %>%
  layer_lstm(units=32, activation="relu", dropout = 0.1, recurrent_dropout = 0.1) %>%
  layer_dense(units=32, activation = "relu") %>%
  layer_dense(units=1, activation = "linear")

model_ls %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = list("mae")
)

model_ls

data_treino = array(data_treino, dim = c(22203, 5, 192))
data_teste = array(data_teste, dim = c(15807, 5, 192))
dim(dados_teste_GHI)

#Treinamento
model_ls %>% fit(
  data_treino, kt15_DNI_treino,
  epochs = 30, batch_size = 64, shuffle = F)

#Avaliação da LSTM
results = model_ls %>% evaluate(data_teste, kt15_DNI_teste)
results
previsao = predict(model_ls, data_teste)
previsao = previsao * kcs15_DNI_teste
métricas(previsao, DNI15_teste)


#Alocação dos resultados
previsao30_LBV_LSTM = previsao
previsao30_sem_LBV_LSTM = previsao

previsao15_LBV_DNI_LSTM = previsao
previsao30_sem_LBV_DNI_LSTM = previsao

write.table(previsao30_LBV_LSTM, file = "previsao30_LBV_LSTM.txt" )
write.table(previsao30_sem_LBV_LSTM, file = "previsao30_sem_LBV_LSTM.txt" )

write.table(previsao15_LBV_DNI_LSTM, file = "previsao15_LBV_DNI_LSTM.txt" )
write.table(previsao30_sem_LBV_DNI_LSTM, file = "previsao30_sem_LBV_DNI_LSTM.txt" )
