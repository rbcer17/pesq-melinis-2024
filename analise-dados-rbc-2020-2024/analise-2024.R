#Analise dados Romero e Hay PNB so chuva sementes sem cobertura
#dados importados da planilha excel como dados numericos nas seguintes
#colunas: ano, ambiente, parcela, coleta, bandeja, total, viaveis, cobmel, cobtot
#
#planilha do drive: consolidado-correto.xlsx
#
#rotina de importacao:
library(readxl)
consolidado <- read_excel("consolidado-correto.xlsx")
consolidado = as.data.frame (consolidado_correto_chuva_sementes_cobertura)
consolidado$pcviaveis = (consolidado$Viaveis / consolidado$Total)*100
#
consolidado$anofactor = as.factor(consolidado$Ano)
consolidado$ambientefactor = as.factor(consolidado$Ambiente)

boxplot(Viaveis~ambientefactor,
        data=consolidado,
        main="Different boxplots for each treatment type",
        xlab="Treatment",
        ylab="Fertile Seeds",
        col="orange",
        border="brown"
)

#boxplots com numero total de sementes e total de viaveis
ggplot(consolidado, aes(x=ambientefactor,y=Viaveis, fill = anofactor))+ geom_boxplot()
ggplot(consolidado, aes(x=ambientefactor,y=Total, fill = anofactor))+ geom_boxplot()
#boxplot com porcentagem de viaveis
ggplot(consolidado, aes(x=ambientefactor,y=pcviaveis, fill = anofactor))+ geom_boxplot()

#Plotar cobertura só
cobertura = as.data.frame(data_only_cobertura_2003_a_2005_sent_04may2020)
boxplot(total03~trat,
        data=cobertura,
        main="Different boxplots for each treatment type",
        xlab="Treatment",
        ylab="Total Coverage",
        col="orange",
        border="brown"
)


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
consolidtudo <- consolidfacarc %>% 
  mutate(naoviaveis= Total - Viaveis)

#agora o dataframe consolidtudo esta completo para as analises de regressao logistica poisson e binomial
#e inclue os dados de cobertura transformados para arcoseno para fazer a ANOVA
#assim como a nova variavel naoviav para fazer a regressao binomial


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
# FAZER POISSON and BINOMIAL LOGISTIC REGRESSION AMBS 3 e 4

poissonreg34 <- glm(formula = Viaveis ~ factor(ambfactor) + factor(anofactor), family = poisson, data = consol34)

binomialreg34 <- glm(formula = cbind(Viaveis,naoviaveis) ~ factor(ambfactor) + factor(anofactor), family = binomial, data = consol34)

print(summary(poissonreg34))
print(summary(binomialreg34))

#-------------------------------------------------
#Depois refazer o glm com uma logistic regression do tipo binomial

#Vou ter de repetir explicitando o dataframe consol34 em vez de ser implicito pois o ypropviav virou variavel externa
# agora vou criar uma variavel percentagem viaveis no consol34 e extrair as linhas
#com valores faltantes
consol34p <- consol34 %>% 
  mutate(pviaveis = (Viaveis/Total))
consol34pnomiss<- na.omit(consol34p)
#nova equacao binomial logistic regression
library(MASS)
poissonregbinom34 <- glm(formula = pviaveis ~ ambfactor + anofactor, family = binomial, data = consol34pnomiss)
poissonregbinom34 <- glm(formula = cbind(Viaveis,naoviaveis) ~ ambfactor + anofactor, family = binomial, data = consol34pnomiss)
poissonregbinom34 <- glm(formula = cbind(Viaveis,naoviaveis) ~ arcobtot, family = binomial, data = consol34pnomiss)

