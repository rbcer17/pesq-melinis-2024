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
#
#agora selecionar so os dados dos grupos 3 e 4
melinistrat34 <- subset(melinisfac, Ambiente == 3 | Ambiente == 4)
melinistrat34
melinistrat12 <- subset(melinisfac, Ambiente == 1 | Ambiente == 2)

resultado2 = lm(Total ~ ambfactor + anofactor,  data =melinistrat34 )
resultado3 = lm(Total ~ ambfactor + anofactor,  data =melinistrat12 )

summary(resultado1)
summary(resultado2)
summary(resultado3)


#agora fazer uns boxplots
#boxplot(crede,sem_rede, main= "Numero de Frutos em Flores Com ou Sem Rede", names = c("Com Rede", "Sem Rede"))
#