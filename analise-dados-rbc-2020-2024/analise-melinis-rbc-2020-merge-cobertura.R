#Analise dados Romero e Hay PNB so chuva sementes sem cobertura
#dados importados da planilha excel como dados numericos nas seguintes
#colunas: ano, ambiente, parcela, coleta, bandeja, total, viaveis
#
#planilha do drive: chuva-sementes-for-r-single-sheet-all-years.xlsx
#
#rotina de importacao:
library(readxl)
chuva_sementes_for_r_single_sheet_all_years <- read_excel("chuva-sementes-for-r-single-sheet-all-years.xlsx")
#
#copiar planilha para objeto melinis
#
melinis <- chuva_sementes_for_r_single_sheet_all_years
View(melinis)

#
#--------------------------------------------------------
#CONVERTER DADOS DE NUMERICO PARA FACTOR
#--------------------------------------------------------
#Todos os dados foram importados como numericos. Agora precisa 
#converter dados numericos para classes usando o comando factor:
# ano, ambiente, coleta
#no momento nao usar as variaveis parcela e bandeja
#usaremos o comando mutate do pacote dplyr
library(dplyr)
melinis2 <- melinis %>% 
  mutate(anofactor = factor(Ano))
melinis3 <- melinis2 %>% 
  mutate(ambfactor = factor(Ambiente))
melinis4 <- melinis3 %>% 
  mutate(coletfactor = factor(Coleta))
melinisfac <- melinis4
#
#remover objetos temporarios
rm(melinis2)
rm(melinis3)
rm(melinis4)
#------------------------------------------
#EXTRAIR E CRIAR NOVAS PLANILHAS SO COM DADOS TRAT 3 e 4 e TRAT 1 e 2
#--------------------------------------------
#agora selecionar so os dados dos grupos 3 e 4
melinistrat34 <- subset(melinisfac, Ambiente == 3 | Ambiente == 4)
melinistrat34
melinistrat12 <- subset(melinisfac, Ambiente == 1 | Ambiente == 2)

resultado2 = lm(Total ~ ambfactor + anofactor,  data =melinistrat34 )
resultado3 = lm(Total ~ ambfactor + anofactor,  data =melinistrat12 )

summary(resultado1)
summary(resultado2)
summary(resultado3)
#-----------------------------------------------------------------
# FAZER POISSON LOGISTIC REGRESSION AMBS 3 e 4

poissonreg34 <- glm(formula = Viaveis ~ factor(ambfactor) + factor(anofactor), family = poisson, data = melinistrat34)
print(summary(poissonreg34))
#-------------------------------------------------
#EXTRAIR SOMA ANUAL DE CADA PARCELA
somaanomelinis34 <-  melinistrat34 %>% group_by (anofactor, ambfactor, Parcela) %>% summarize(anoviaveis = sum(Viaveis, na.rm =TRUE))
#--------------------------------------------------------------------
#
#IMPORTAR TABELA COBERTURA E DEPOIS JUNTAR COM A PLANILHA DE SEMENTES
data_only_cobertura_2003_a_2005_sent_04may2020 <- read_excel("data-only-cobertura 2003 a 2005-sent-04may2020.xlsx")
cobertura1 <- data_only_cobertura_2003_a_2005_sent_04may2020
#----------------------------------------------------------------
#MELT FUNCTION PARA DESDOBRAR TABELA DE COBERTURA
#stackoverflow.com melt-multiple-id-vars-as-all-columns-excluded-from-a-list
#
library(reshape2)
varsid1 <- c("trat", "parcela")
cobertura2 <- melt(cobertura1, id.vars = varsid1)
names(cobertura2)[3]<- "codigo"
names(cobertura2)[4]<- "percen"
names(cobertura2)[2]<- "Parcela"
names(cobertura2)[1]<- "Ambiente"
#AGORA CRIAR AS IDS ANO E TIPO DE COBERTURA NO DATAFRAME cobertura2
#-----------------------------------------------------------------
#PRIMEIRO IMPORTAR A PLANILHA LEGENDA E FAZER UM MERGE COM cobertura2
legenda_codigos_cobertura <- read_excel("legenda-codigos-cobertura.xlsx")
legenda1 <- legenda_codigos_cobertura
cobertura3 <- merge(cobertura2,legenda1)
#FAZER UM CAST LIBRARY RESHAPE CRIANDO novo dataframe cobertura4 com AS NOVAS VARIAVEIS COBMEL E COBTOT A PARTIR DE ESPECIE E PERCEN
#Comando cast desdobra dataframe transformando uma variavel em novas de acordo com
#as categorias da variavel original e dando o valor da variavel nova de acordo com o subcomando value
library(reshape)
cobertura4 <- cast(cobertura3, Ano + Ambiente + Parcela ~ especie, value="percen")
#AGORA FAZER UM MERGE DE CHUVA SEMENTES melinis E COBERTURA cobertura3
#USANDO OS CAMPOS Ano Parcela Ambiente
#------------------------------------------------------------
meliniscob <- merge(melinis, cobertura4, by=c("Ano","Ambiente","Parcela"))
#-----------------------------------------------------------------
#FAZER AGORA NOVA POISSON LOGISTIC REGRESSION incluindo as variaveis de cobertura
#Todos os dados foram importados como numericos. Agora precisa 
#converter dados numericos para classes usando o comando factor:
# ano, ambiente, coleta
#no momento nao usar as variaveis parcela e bandeja
#usaremos o comando mutate do pacote dplyr
library(dplyr)
meliniscob2 <- meliniscob %>% 
  mutate(anofactor = factor(Ano))
meliniscob3 <- meliniscob2 %>% 
  mutate(ambfactor = factor(Ambiente))
meliniscob4 <- meliniscob3 %>% 
  mutate(coletfactor = factor(Coleta))
meliniscobfac <- meliniscob4
library(plyr)
count(meliniscobfac$ambfactor)
#
#remover objetos temporarios
rm(meliniscob2)
rm(meliniscob3)
rm(meliniscob4)
#------------------------------------------
#EXTRAIR E CRIAR NOVAS PLANILHAS SO COM DADOS TRAT 3 e 4 e TRAT 1 e 2
#--------------------------------------------
#agora selecionar so os dados dos grupos 3 e 4
meliniscobtrat34 <- subset(meliniscobfac, Ambiente == 3 | Ambiente == 4)
meliniscobtrat34
meliniscobtrat12 <- subset(meliniscobfac, Ambiente == 1 | Ambiente == 2)
#------------------------------------------------------------
#-----------------------------------------------------------------
# FAZER POISSON LOGISTIC REGRESSION AMBS 3 e 4

poissonregcob34 <- glm(formula = Viaveis ~ factor(ambfactor) + factor(anofactor) + cobmel, family = poisson, data = meliniscobtrat34)
print(summary(poissonregcob34))
#----------------------------------------------
#criar nova variavel percviav na planilha meliniscobtrat24
#-------------------------------------------------
library(tidyverse)
melinispercviav34 <-meliniscobtrat34 %>% 
mutate(percviav = Viaveis/Total)
poissonregpercviav34 <- glm(formula = percviav ~ factor(ambfactor) + factor(anofactor) + cobmel, family = poisson, data = melinispercviav34)
print(summary(poissonregpercviav34))

#EXTRAIR SOMA ANUAL DE CADA PARCELA
somaanomelinis34 <-  melinistrat34 %>% group_by (anofactor, ambfactor, Parcela) %>% summarize(anoviaveis = sum(Viaveis, na.rm =TRUE))
#--------------------------------------------------------------------
#