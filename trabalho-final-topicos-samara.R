#Previsão de Poluição e Regressão Linear

#Carregar pacotes
install.packages("ggplot2")
install.packages("car")
install.packages("corrplot")
library(ggplot2)
library(car)
library(corrplot)

#Y = poluição e X = temperatura

# Criar DataFrame com os dados
dados <- data.frame(
  Poluicao = c(24,30,56,28,14,46,9,35,26,61,29,28,14,18,17,23,47,13,31,12,10,110,56,10,69,8,36,16,29,29,65,9,10,26,31,10,11,14,17,11,94),
  Temp = c(61.5,55.6,55.9,51,68.4,47.6,66.2,49.9,57.8,50.4,57.3,52.3,51.5,59.4,51.9,54,55,61,55.2,56.7,70.3,50.6,49.1,68.9,54.6,56.6,54,45.7,51.1,43.5,49.7,68.3,75.5,51.5,59.3,61.6,47.1,54.5,49,56.8,50)
)

# Visualizar os dados
head(dados)
summary(dados)

# Criar gráfico de dispersão
ggplot(dados, aes(x=Temp, y=Poluicao)) + 
  geom_point(color='blue') + 
  geom_smooth(method='lm', color='red', se=FALSE) +
  labs(title='Relação entre Temperatura e Poluição', x='Temperatura', y='Poluição')


#Relação Identificada: Relação inversa entre temperatura e poluição. A temperatura não é a única determinante da poluição. 

# Criar DataFrame com os dados outros dados
dados <- data.frame(
  Poluicao = c(24,30,56,28,14,46,9,35,26,61,29,28,14,18,17,23,47,13,31,12,10,110,56,10,69,8,36,16,29,29,65,9,10,26,31,10,11,14,17,11,94),
  Temp = c(61.5,55.6,55.9,51,68.4,47.6,66.2,49.9,57.8,50.4,57.3,52.3,51.5,59.4,51.9,54,55,61,55.2,56.7,70.3,50.6,49.1,68.9,54.6,56.6,54,45.7,51.1,43.5,49.7,68.3,75.5,51.5,59.3,61.6,47.1,54.5,49,56.8,50),
  Industria = c(368,291,775,137,136,44,641,1064,197,347,434,361,181,275,454,462,625,91,35,453,213,3344,412,721,1692,125,80,569,379,669,1007,204,207,266,96,337,391,381,104,46,343),
  Populacao = c(497,593,622,176,529,116,844,1513,299,520,757,746,347,448,515,453,905,132,71,716,582,3369,158,1233,1950,277,80,717,531,744,751,361,335,540,308,624,463,507,201,244,179)
)


# Visualizar os dados
head(dados)
summary(dados)

#Criar gráfico de dispersão comparando a poluição e a população
ggplot(dados, aes(x=Poluicao, y=Populacao)) + 
  geom_point(color='blue') + 
  geom_smooth(method='lm', color='red', se=FALSE) +
  labs(title='Relação entre Poluição e População', x='Poluicao', y='Populacao')

#Aqui vemos que quanto maior a população há um aumento na poluição.

#Criar gráfico de dispersão comparando a poluição e a industria
ggplot(dados, aes(x=Poluicao, y=Industria)) + 
  geom_point(color='blue') + 
  geom_smooth(method='lm', color='red', se=FALSE) +
  labs(title='Relação entre Poluicao e Industria', x='Poluicao', y='Industria')

#Aqui vemos que quanto maior a industria há um aumento na poluição.

# Agora vamos ajustar o modelo para a regressão linear
modelo <- lm(Poluicao ~  Temp + Industria + Populacao, data = dados)

# Plotar resíduos para verificar o ajuste do modelo
par(mfrow = c(2, 2)) # Criar múltiplos gráficos em uma janela
plot(modelo)
shapiro.test(modelo$residuals)

#Os resíduos são a diferença entre os valores observados e os valores ajustados pelo modelo.
##transforma variáveis independentes em previsões e como ele ajusta os coeficientes para minimizar os erros.

#Conclusão: Os gráficos não atende às premissas da regressão linear. 
##Para resolver é preciso que haja uma reanálise dos dados, pois os resultados podem ser enganosos. 
# O código pode ser usado para resolver outros problemas básicos da biologia, como a relação entre variáveis ambientais, populacionais e ecológicas.


