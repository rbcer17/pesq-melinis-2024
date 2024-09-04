#Analise dados Romero e Hay PNB so chuva sementes sem cobertura
#dados importados da planilha excel como dados numericos nas seguintes
#colunas: ano, ambiente, parcela, coleta, bandeja, total, viaveis, cobmel, cobtot
#
#planilha do drive: consolidado.xlsx
#
#rotina de importacao:
library(readxl)
consolidado <- read_excel("consolidado.xlsx")
#
#Criar variaveis fator para ano, ambiente e coleta
#usaremos o comando mutate do pacote dplyr
library(dplyr)
cons2 <- consolidado %>% 
  mutate(anofactor = factor(Ano))
cons3 <- cons2 %>% 
  mutate(ambfactor = factor(Ambiente))
cons4 <- cons3 %>% 
  mutate(coletfactor = factor(Coleta))
consolidfac <- cons4
#
#remover objetos temporarios
rm(cons2)
rm(cons3)
rm(cons4)
#fazer transformacao arcoseno para cobertura
cons5 <- consolidfac %>% 
  mutate(arcobmel= asin(sqrt(cobmel / 100)))
cons6 <- cons5 %>% 
  mutate(arcobtot= asin(sqrt(cobtot / 100)))
consolidfacarc <- cons6
#remover objetos temporarios
rm(cons5)
rm(cons6)
#criar nova variavel naoviav para analise binomial
consolidtu1 <- consolidfacarc %>% 
  mutate(naoviaveis= Total - Viaveis)
#criar uma nova variavel ypropviav que e uma matriz de 2 colunas viaveis e nao viaveis
# nao funciona assim
#consolidtudo <- consolidtu1 %>% 
#mutate(ypropviav = cbind(Viaveis, naoviaveis))

#o cbind nao funcionou vou ter de ver outra forma de fazer este binomial
#vou tentar criar uma variavel externa e repetir o modelo la embaixo explicitando o dataframe
consolidtudo <- consolidtu1
ypropviav = cbind(consolidtudo$Viaveis, consolidtudo$naoviaveis)


#agora o dataframe consolidtudo esta completo para as analises de regressao logistica poisson e binomial
#e inclue os dados de cobertura transformados para arcoseno para fazer a ANOVA
#assim como a nova variavel ypropviav para fazer a regressao binomial


#------------------------------------------

#------------------------------------------
#EXTRAIR E CRIAR NOVAS PLANILHAS SO COM DADOS TRAT 3 e 4 e TRAT 1 e 2
#--------------------------------------------
#agora selecionar so os dados dos grupos 3 e 4
consol34 <- subset(consolidtudo, Ambiente == 3 | Ambiente == 4)
consol34
consol12 <- subset(consolidtudo, Ambiente == 1 | Ambiente == 2)
consol12
#
# FAZER POISSON LOGISTIC REGRESSION AMBS 3 e 4

poissonreg34 <- glm(formula = Viaveis ~ factor(ambfactor) + factor(anofactor), family = poisson, data = consol34)
poissonreg34 <- glm(formula = Total ~ factor(ambfactor) + factor(anofactor), family = poisson, data = consol34)
poissonreg34 <- glm(formula = cbind(Viaveis,naoviaveis) ~ factor(ambfactor) + factor(anofactor), family = binomial(link = "cloglog"), data = consol34p)

print(summary(poissonreg34))
#-------------------------------------------------
#Depois refazer o glm com uma logistic regression do tipo binomial
poissonregbinom34 <- glm(formula = ypropviav ~ factor(ambfactor) + factor(anofactor), family = binomial, data = consol34)
#Vou ter de repetir explicitando o dataframe consol34 em vez de ser implicito pois o ypropviav virou variavel externa
# agora vou criar uma variavel percentagem viaveis no consol34 e extrair as linhas
#com valores faltantes
consol34p <- consol34 %>% 
  mutate(pviaveis = (Viaveis/Total))
consol34pnomiss<- na.omit(consol34p)
#nova equacao binomial logistic regression
library(MASS)
poissonregbinom34 <- glm(formula = pviaveis ~ ambfactor + anofactor, family = binomial, data = consol34pnomiss)
poissonregbinom34 <- glm(formula = cbind(Viaveis,naoviaveis) ~ ambfactor + anofactor, family = quasibinomial(), data = consol34pnomiss)
poissonregbinom34 <- glm(formula = cbind(Viaveis,naoviaveis) ~ arcobtot, family = binomial, data = consol34pnomiss)
poissonregbinom34 <- glmmPQL(formula = cbind(Viaveis,naoviaveis) ~ ambfactor + anofactor, random=~+1|Hamster, family= binomial, data = consol34pnomiss)

ypropviav34 <- cbind(consol34$Viaveis, consol34$naoviaveis)
yproptot<- consol34$Viaveis / (consol34$Viaveis + consol34$naoviaveis)
View(yproptot)
poissonregbinom34 <- glm(formula = yproptot ~ factor(consol34$ambfactor) + factor(consol34$anofactor), family = binomial)
print(summary(poissonregbinom34))
