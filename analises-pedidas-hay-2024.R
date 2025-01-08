consolidado = as.data.frame (consolidado_correto_chuva_sementes_cobertura)
consolidado$pcviaveis = (consolidado$Viaveis / consolidado$Total)*100
consolidado$anofactor = as.factor(consolidado$Ano)
consolidado$ambientefactor = as.factor(consolidado$Ambiente)
consolidado$coletafactor = as.factor(consolidado$Coleta)



consolidado$zerosem = bandejazero(consolidado$Total)
#Comparar chuva sementes ambiente 1 vs 2 (LC vs HC) pag 7 e pag 11
#Comparar chuva sementes ambiente 3 vs 4 (FM vs IMM) pag 7
#fazer primeiro o boxplot 1 vs 2 por ano
#e o boxplot 3 vs 4 por ano
consol34 <- subset(consolidado, Ambiente == 3 | Ambiente == 4)
consol12 <- subset(consolidado, Ambiente == 1 | Ambiente == 2)
#boxplot percent viables 1 2
ggplot(consol12, aes(x=ambientefactor,y=pcviaveis, fill = anofactor))+ geom_boxplot()
#boxplot total seed rain 1 3
ggplot(consol12, aes(x=ambientefactor,y=Total, fill = anofactor))+ geom_boxplot()

#boxplot percent viables 3 4
ggplot(consol34, aes(x=ambientefactor,y=pcviaveis, fill = anofactor))+ geom_boxplot()

#boxplot total seed rain 3 4 
ggplot(consol34, aes(x=ambientefactor,y=Total, fill = anofactor))+ geom_boxplot()

#Comparar chuva sementes 1 2 transformar poisson
poissonreg34 <- glm(formula = Viaveis ~ factor(ambientefactor) + factor(anofactor), family = poisson, data = consol34)
summary(poissonreg34)
#Comparar chuva sementes 3 4 transformar poisson
poissonreg34 <- glm(formula = Viaveis ~ factor(ambfactor) + factor(anofactor), family = poisson, data = consol34)

#Comparar percent viaveis 1 2 transformar arcoseno ou melhor fazer binomial
#arcoseno so funciona se os dados nao tiverem muitos zero ou 100

#Comparar  percent viaveis 3 4 transformar arcoseno ou melhor fazer binomial
library(MASS)
poissonregbinom34 <- glm(formula = pviaveis ~ ambfactor + anofactor, family = binomial, data = consol34pnomiss)
poissonregbinom34 <- glm(formula = cbind(Viaveis,naoviaveis) ~ ambfactor + anofactor, family = binomial, data = consol34pnomiss)
poissonregbinom34 <- glm(formula = cbind(Viaveis,naoviaveis) ~ arcobtot, family = binomial, data = consol34pnomiss)

#for percent viables use binomial regression with cbind for proportions

#analise percentagem viaveis dentro mesmo ano para cada tratamento
consol1 <- subset(consol12, Ambiente == 1)
consol2 <- subset(consol12, Ambiente == 2)
consol3 <- subset(consol34, Ambiente == 3)
consol4 <- subset(consol34, Ambiente == 4)
#boxplots percentagem viaveis dentro mesmo ano
par(mfrow=c(2,2))
ggplot(consol1, aes(x=coletafactor,y=pcviaveis, fill = anofactor))+ geom_boxplot()
ggplot(consol2, aes(x=coletafactor,y=pcviaveis, fill = anofactor))+ geom_boxplot()
ggplot(consol3, aes(x=coletafactor,y=pcviaveis, fill = anofactor))+ geom_boxplot()
ggplot(consol4, aes(x=coletafactor,y=pcviaveis, fill = anofactor))+ geom_boxplot()

#analise percentagem viaveis dentro mesmo ano para tratamentos 1 vs 2 e 3 vs 4



#analise numero bandejas com zero sementes ao longo do tempo
#criar nova variavel a partir da variavel Total (numero sementes)
# variavel zerosem com dois valores: 1 se nao tiver sementes, 0 se tiver
#######
#TEM DE REFAZER A ANALISE COM ZERO SEMENTES VIAVEIS (NAO TOTAL)
consolidado$zerosem = ifelse (consolidado$Total == 0, 1, 0)
freq_table <- consolidado %>%
  group_by(Ambiente,Coleta, zerosem) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))

contagemzero = as.data.frame(freq_table)
socontagemzero = subset(contagemzero,zerosem == 1)
socontagemzero$Coleta=as.factor(socontagemzero$Coleta)

ggplot(socontagemzero, aes(fill=Coleta, y=frequency, x=Ambiente)) + 
  geom_bar(position="dodge", stat="identity")

#analise bandejas com zero sementes viaveis ao longo do tempo
consolidado$zeroviaveis = ifelse (consolidado$Viaveis == 0, 1, 0)
freq_table2 <- consolidado %>%
  group_by(Ambiente,Coleta, zeroviaveis) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))

contagemzeroviaveis = as.data.frame(freq_table2)
socontagemzeroviaveis = subset(contagemzeroviaveis,zeroviaveis == 1)
socontagemzeroviaveis$Coleta=as.factor(socontagemzeroviaveis$Coleta)

ggplot(socontagemzeroviaveis, aes(fill=Coleta, y=frequency, x=Ambiente)) + 
  geom_bar(position="dodge", stat="identity")

zeroviaveis1 <- subset(consolidado, Ambiente == 1)
# Fazer a figura 2 no R  boxplot de cobertura total e cobertura melinis por tratamento por ano
#4 boxplots sendo cada um cobertura total e cobertura melinis por ano ,  um boxplot por tratamento
# one box per variety
p2 <- ggplot(consolidado, aes(x=variety, y=note, fill=treatment)) + 
  geom_boxplot() +
  facet_wrap(~variety, scale="free")
#
# Vamos tentar outra forma, com par mfrow
#https://bookdown.org/ndphillips/YaRrr/arranging-plots-with-parmfrow-and-layout.html

par(mfrow = c(2, 2)) # Create a 2 x 2 plotting matrix
# The next 4 plots created will be plotted next to each other

#for ggplot need to use function grid.arrange
#https://stackoverflow.com/questions/1249548/side-by-side-plots-with-ggplot2
require(gridExtra)
plot1=ggplot(consolidado, aes(x=anofactor,y=pcviaveis, fill = anofactor))+ geom_boxplot()
plot2=ggplot(consolidado, aes(x=anofactor,y=pcviaveis, fill = anofactor))+ geom_boxplot()
plot3=ggplot(consolidado, aes(x=anofactor,y=pcviaveis, fill = anofactor))+ geom_boxplot()
plot4=ggplot(consolidado, aes(x=anofactor,y=pcviaveis, fill = anofactor))+ geom_boxplot()


grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

#pivot longer for cobertura
library(tidyr)
#https://www.statology.org/pivot_longer-in-r/
#pivot the data frame into a long format
coberturapivot = coberturapivot %>% pivot_longer(cols=c('total03', 'total04','total05'),
                    names_to='ano',
                    values_to='cobtotal')
coberturapivot = coberturapivot %>% pivot_longer(cols=c('mm03', 'mm04','mm05'),
                                                 names_to='anomm',
                                                 values_to='cobmm')
coberturapivot = coberturapivot %>% pivot_longer(cols=c('cobtotal', 'cobmm'),
                                                 names_to='tipocob',  values_to='percob')
#remove first 5 characters from ano variable
coberturapivot$ano2 = substr(coberturapivot$ano,6,7)
#remove colums 3 and 4 from dataframe
coberturafinal <- coberturapivot[ -c(3:4) ]
#remove duplicated rows
cobfinalded <- coberturafinal[!duplicated(coberturafinal), ]

#for ggplot need to use function grid.arrange
#https://stackoverflow.com/questions/1249548/side-by-side-plots-with-ggplot2
require(gridExtra)
plot1=ggplot(coberturafinal, aes(x=ano2,y=percob, fill = tipocob))+ geom_boxplot()
plot2=ggplot(coberturafinal, aes(x=ano2,y=percob, fill = tipocob))+ geom_boxplot()
plot3=ggplot(coberturafinal, aes(x=ano2,y=percob, fill = tipocob))+ geom_boxplot()
plot4=ggplot(coberturafinal, aes(x=ano2,y=percob, fill = tipocob))+ geom_boxplot()


grid.arrange(plot1, plot2, plot3, plot4, ncol=2)
