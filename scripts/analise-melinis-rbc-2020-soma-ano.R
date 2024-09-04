#Analise dados Romero e Hay PNB so chuva sementes sem cobertura
#dados importados da planilha excel como dados numericos nas seguintes
#colunas: ano, ambiente, parcela, coleta, bandeja, total, viaveis
#
#planilha do drive: chuva-sementes-for-r-single-sheet-all-years.xlsx
#
#rotina de importacao:
#library(readxl)
#chuva_sementes_for_r_single_sheet_all_years <- read_excel("chuva-sementes-for-r-single-sheet-all-years.xlsx")
#
#copiar planilha para objeto melinis
#
melinis <- chuva_sementes_for_r_single_sheet_all_years
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


------------------------------------------------------
#FAZER GRAFICOS
#-------------------------------------------------
#agora fazer uns boxplots
#boxplot(crede,sem_rede, main= "Numero de Frutos em Flores Com ou Sem Rede", names = c("Com Rede", "Sem Rede"))
#
# Tabelar frequencias exemplo para agrupar variavel continua por grupo
#library(dplyr)
#group_by(namedataframe, groupingvariable) %>%
#  summarise(
#    count = n(),
#    median = median(continuousvariable, na.rm = TRUE),
#    IQR = IQR(score, na.rm = TRUE)
#  )
library(dplyr)
group_by(melinistrat34, ambfactor) %>%
  summarise(
    count = n(),
    median = median(Viaveis, na.rm = TRUE),
    IQR = IQR(Viaveis, na.rm = TRUE)
  )

# Plot score by posicao and color by posicao
library("ggpubr")
ggboxplot(melinistrat34, x = "ambfactor", y = "Viaveis", 
          color = "ambfactor", palette = c("#00AFBB", "#E7B800"),
          ylab = "Viaveis", xlab = "Trat")
#acertar os niveis para boxplot
melinistrat34$ambfactor<-factor(melinistrat34$ambfactor, levels=c("3","4"))
melinistrat34$anofactor<-factor(melinistrat34$anofactor, levels=c("2003","2004","2005"))

boxplot(Viaveis~ambfactor+anofactor, melinistrat34)
boxplot(Viaveis~interaction(ambfactor, anofactor, lex.order=T), melinistrat34)

#---------------------------------------------------------------
#RODAR MODELO ANOVA COM DADOS TRATS 3 e 4
#-----------------------------------------------------
#Homocedasticidade e Normalidade
#-------------------------------------------------------
# Usaremos o test de levene que calcula a homocedasticidade
# H0 = variancia entre os grupos igual
# leveneTest(y = vetor numerico, group = fator dos dados)
#install.packages("car")
#library(car)
#
#leveneTest(frutos$prod~frutos$fat)
#----------------------------------------------------------
#Normalidade
#Testar a posteriori olhando a normalidade dos residuos
#shapiro.test(resid(resultadoanova))
#----------------------------------------------------------
#Testar quais classes contribuem para efeitos significativos
#------------------------------------------------------------
#Tukey Post hoc test para ver diferencas entre grupos
#Test post hoc - Teste de Tukey ----
  # Observamos que ha variacao entre as medias, mas quais medias
  # variam entre elas? Sera que todas sao diferentes ou nao?
  
#posth <-TukeyHSD(resultado2)
#posth
#plot(posth)
#Quando a barra cruzar o zero no eixo x, significa que a diferenca
# entre esses grupos nao e significativo. 
# Se a diferencia for positiva, a media do primeiro fator e maior 
# que do segundo fator.
#
# Quais sao as medias que nao tem diferencias significativas?
#
#Teste Kruskal wallis ----
# 
# Visto se tem diferenca ou nao, devemos verificar onde esta a diferenca
# Vamos usar um Tukey nao parametrico para o kruskal wallis
# pairwise.wilcox.test(Planilha_galhas$N_galhas,Planilha_galhas$Planta)
#---------------------------------------------------------------

