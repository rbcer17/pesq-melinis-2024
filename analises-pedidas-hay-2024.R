consolidado = as.data.frame (consolidado_correto_chuva_sementes_cobertura)
consolidado$pcviaveis = (consolidado$Viaveis / consolidado$Total)*100
consolidado$anofactor = as.factor(consolidado$Ano)
consolidado$ambientefactor = as.factor(consolidado$Ambiente)
consolidado$coletafactor = as.factor(consolidado$Coleta)

#FAZER FIGS 1 e 3 FINAL FORMATO MS ver pasta geral do projeto


consolidado$zerosem = bandejazero(consolidado$Total)
#Comparar chuva sementes ambiente 1 vs 2 (LC vs HC) pag 7 e pag 11
#Comparar chuva sementes ambiente 3 vs 4 (FM vs IMM) pag 7
#fazer primeiro o boxplot 1 vs 2 por ano
#e o boxplot 3 vs 4 por ano
consol12 <- subset(consolidado, Ambiente == 1 | Ambiente == 2)
consol34anos20042005 <- subset(consol34, Ano == 2004 | Ano == 2005)
#boxplot percent viables 1 2
ggplot(consol12, aes(x=ambientefactor,y=pcviaveis, fill = anofactor))+ geom_boxplot()
#boxplot total seed rain 1 3
ggplot(consol12, aes(x=ambientefactor,y=Total, fill = anofactor))+ geom_boxplot()
ggplot(consolidado, aes(x=ambientefactor,y=Viaveis, fill = anofactor))+ geom_boxplot()

#fig 1 final com labels corretos
ggplot(consolidado, aes(x=ambientefactor,y=Viaveis, fill = anofactor))+ geom_boxplot() + labs( x = "Treatment", y = "Number of full caryopses", fill = "Year")

#boxplot percent viables 3 4
ggplot(consol34, aes(x=ambientefactor,y=pcviaveis, fill = anofactor))+ geom_boxplot()

#boxplot total seed rain 3 4 
ggplot(consol34, aes(x=ambientefactor,y=Total, fill = anofactor))+ geom_boxplot()

#Comparar chuva sementes 1 2 transformar poisson
poissonreg12 <- glm(formula = Viaveis ~ factor(ambientefactor) + factor(anofactor), family = poisson, data = consol12)
summary(poissonreg12)
anova(poissonreg12)
#Comparar chuva sementes 3 4 transformar poisson
poissonreg34 <- glm(formula = Viaveis ~ factor(ambfactor) + factor(anofactor), family = poisson, data = consol34)
poissonreg34so2anos <- glm(formula = Viaveis ~ factor(ambientefactor) + factor(anofactor), family = poisson, data = consol34anos20042005)
summary(poissonreg34so2anos)
shapiro.test(poissonreg34so2anos$residuals)
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
poissonreg1 <- glm(formula = Viaveis ~  factor(coletafactor), family = poisson, data = consol1)
summary(poissonreg1)
shapiro.test(poissonreg1$residuals)
#Kruskal Wallis viaveis vs data de coleta
kruskal.test(Viaveis ~ coletafactor,
             data = consol1
)
kruskal.test(Viaveis ~ coletafactor,
             data = consol2
)
kruskal.test(Viaveis ~ coletafactor,
             data = consol3
)
kruskal.test(Viaveis ~ coletafactor,
             data = consol4
)

#boxplots percentagem viaveis dentro mesmo ano
par(mfrow=c(2,2))

#Fig 05 melinis manuscript
require(gridExtra)
plot01=ggplot(consol1, aes(x=coletafactor,y=pcviaveis, fill = anofactor))+ geom_boxplot() + labs(title = "A", x = "Treatment", y = "Percent of full caryopses", fill = "Year")
plot02=ggplot(consol2, aes(x=coletafactor,y=pcviaveis, fill = anofactor))+ geom_boxplot() + labs( title = "B", x = "Treatment", y = "Percent of full caryopses", fill = "Year")
plot03=ggplot(consol3, aes(x=coletafactor,y=pcviaveis, fill = anofactor))+ geom_boxplot() + labs(title = "C" ,x = "Treatment", y = "Percent of full caryopses", fill = "Year")
plot04=ggplot(consol4, aes(x=coletafactor,y=pcviaveis, fill = anofactor))+ geom_boxplot() + labs(title = "D", x = "Treatment", y = "Percent of full caryopses", fill = "Year")
grid.arrange(plot01, plot02, plot03, plot04, ncol=2)
#analise percentagem viaveis dentro mesmo ano para tratamentos 1 vs 2 e 3 vs 4

#ANALISE Manuscrito FINAL comparar kruskal wallis tratamentos 3 vs 4 anos 2004 2005 percentagem viaveis , e numerototal
kruskal.test(pcviaveis ~ ambientefactor,
             data = consol34anos20042005
)
kruskal.test(Total ~ ambientefactor,
             data = consol34anos20042005
)

# Dentro de cada tratamento comparar o numero total por coleta e ano 2 way
poissonreg1 <- glm(formula = Viaveis ~ factor(coletafactor) + factor(anofactor), family = poisson, data = consol1)
summary(poissonreg1)
anova(poissonreg1)
# Dentro de cada tratamento comparar o numero de viaveis por coleta e ano 2 way
poissonreg2 <- glm(formula = Viaveis ~ factor(coletafactor) + factor(anofactor), family = poisson, data = consol2)
summary(poissonreg2)
anova(poissonreg2)
poissonreg3 <- glm(formula = Viaveis ~ factor(coletafactor) + factor(anofactor), family = poisson, data = consol3)
summary(poissonreg3)
anova(poissonreg3)
poissonreg4 <- glm(formula = Viaveis ~ factor(coletafactor) + factor(anofactor), family = poisson, data = consol4)
summary(poissonreg4)
anova(poissonreg4)

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

# Create boxplot with percentagem viaveis por coleta por ano tratamentos 1 e 2 fig 07
ggplot(consol12, aes(x=coletafactor,y=pcviaveis, fill = anofactor))+ geom_boxplot() + labs( x = "Collection", y = "Percentage of full caryopses", fill = "Year") + ggtitle("A")

contagemzero = as.data.frame(freq_table)
socontagemzero = subset(contagemzero,zerosem == 1)
socontagemzero$Coleta=as.factor(socontagemzero$Coleta)

ggplot(socontagemzero, aes(fill=Coleta, y=frequency, x=Ambiente)) + 
  geom_bar(position="dodge", stat="identity")


#analise bandejas com zero sementes viaveis ao longo do tempo
#refazer a figura com as legendas certas
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

#figure 4 final manuscript
ggplot(socontagemzeroviaveis, aes(fill=Coleta, y=frequency, x=Ambiente)) + 
  geom_bar(position="dodge", stat="identity") + labs( x = "Treatment", y = "Number of trays", fill = "Collection")


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
#
#
#tem de recalcular este trecho para gerar a cobertura media por ano total e melinis por tratamento
#pivot longer for cobertura
library(tidyverse)
#https://www.statology.org/pivot_longer-in-r/
#pivot the data frame into a long format
coberturapivot = as.data.frame(data_only_cobertura_2003_a_2005_sent_04may2020)
coberturapivot1 = coberturapivot %>% pivot_longer(cols=c('total03', 'total04','total05'),
                    names_to='ano',
                    values_to='cobtotal')
coberturapivot2 = coberturapivot %>% pivot_longer(cols=c('mm03', 'mm04','mm05'),
                                                 names_to='anomm',
                                                 values_to='cobmm')
#remove columns 1 to 5 of coberturapivot2
coberturapivot2 <- coberturapivot2[ -c(1:5) ]
#add columns dataframe coberturapivot2 to coberturapivot1
df2 <- cbind(coberturapivot1, coberturapivot2)
#recode ano to have just numbers
df2$anocal = substr(df2$ano,6,7)
#pivot to have cobertura single variable
df3 = df2 %>% pivot_longer(cols=c('cobtotal', 'cobmm'),
                                                  names_to='tipocob',
                                                  values_to='percob')
#remove cols 3 4 5 6 7 from df3
df4 <- df3 [ -c(3:7) ]

#agora vamos usar o dataframe df4 para replicar a figura 3 do paper com sigmaplot
df4$tipocob <- factor(df4$tipocob, levels = c("cobtotal", "cobmm"))
#mydat_tibble <- mydat_tibble %>% 
#  rename(Years_From_Diagnosis = Yrs_From_Dx)

con1 <- subset(df4, trat == 1)
con2 <- subset(df4, trat == 2)
con3 <- subset(df4, trat == 3)
con4 <- subset(df4, trat == 4)
require(gridExtra)
#BOXPLOTS
plot1=ggplot(con1, aes(x=anocal,y=percob, fill = tipocob))+ geom_boxplot()
plot2=ggplot(con2, aes(x=anocal,y=percob, fill = tipocob))+ geom_boxplot()
plot3=ggplot(con3, aes(x=anocal,y=percob, fill = tipocob))+ geom_boxplot()
plot4=ggplot(con4, aes(x=anocal,y=percob, fill = tipocob))+ geom_boxplot()

plot1
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)
#BARPLOT WITH ERROR BARS FIGURE 3 FINAL
#calculate mean values of y
library(dplyr)
library(ggplot2)
summary_data2 <- df4 %>%
  group_by(trat,anocal,tipocob) %>%
  summarize(
    percent_cover = mean(percob),
    sd_value = sd(percob)
  )
#Rename the variables according to the plot
summary_data2 <- summary_data2 %>% rename(coverage = tipocob)
summary_data2 <- summary_data2 %>% rename(year = anocal)
library(forcats)
summary_data2$coverage <- fct_recode(summary_data2$coverage,
                         "total" = "cobtotal",  # Renaming "cobtotal" to "total"
                         "melinis" = "cobmm")  # Renaming "cobmm" to "melinis"
summary_data2$year <- fct_recode(summary_data2$year,
                                     "2003" = "03",  # Renaming "03" to "2003"
                                     "2004" = "04", # Renaming "04" to "2004"
                                     "2005" = "05")  # Renaming "05" to "2005"


#Now do the plots FIGURE 3 FINAL
conb1 <- subset(summary_data2, trat == 1)
conb2 <- subset(summary_data2, trat == 2)
conb3 <- subset(summary_data2, trat == 3)
conb4 <- subset(summary_data2, trat == 4)

bplot1 =ggplot(conb1,  aes(x = year, y = percent_cover, fill = coverage))+
  geom_col( position = "dodge", width = 0.5, alpha = 0.5, color = "black", size = 0.1) + geom_errorbar(aes(ymin = percent_cover-sd_value, ymax = percent_cover+sd_value),
                                                                                                       position =  position_dodge(width = 0.5), width = 0.2)  + ggtitle("A") + labs( x = "Year", y = "Percent Cover")                                                                                                
bplot2 =ggplot(conb2,  aes(x = year, y = percent_cover, fill = coverage))+
  geom_col( position = "dodge", width = 0.5, alpha = 0.5, color = "black", size = 0.1) + geom_errorbar(aes(ymin = percent_cover-sd_value, ymax = percent_cover+sd_value),
                                                                                                       position =  position_dodge(width = 0.5), width = 0.2)   + ggtitle("B") + labs( x = "Year", y = "Percent Cover")                                                                                               
bplot3 =ggplot(conb3,  aes(x = year, y = percent_cover, fill = coverage))+
  geom_col( position = "dodge", width = 0.5, alpha = 0.5, color = "black", size = 0.1) + geom_errorbar(aes(ymin = percent_cover-sd_value, ymax = percent_cover+sd_value),
                                                                                                       position =  position_dodge(width = 0.5), width = 0.2)  + ggtitle("C")  + labs( x = "Year", y = "Percent Cover")                                                                                               
bplot4 =ggplot(conb4,  aes(x = year, y = percent_cover, fill = coverage))+
  geom_col( position = "dodge", width = 0.5, alpha = 0.5, color = "black", size = 0.1) + geom_errorbar(aes(ymin = percent_cover-sd_value, ymax = percent_cover+sd_value),
                                                                                                       position =  position_dodge(width = 0.5), width = 0.2)   + ggtitle("D")  + labs( x = "Year", y = "Percent Cover")                                                                                              
grid.arrange(bplot1, bplot2, bplot3, bplot4, ncol=2)
#to do for final:
# y axis % cover
# x axis full year
# remove tipo cobertura side bar
#coberturapivot = coberturapivot %>% pivot_longer(cols=c('cobtotal', 'cobmm'),
 #                                                names_to='tipocob',  values_to='percob')
#remove first 5 characters from ano variable
coberturapivot$ano2 = substr(coberturapivot$ano,6,7)
#remove colums 3 and 4 from dataframe
coberturafinal <- coberturapivot[ -c(3:4) ]
#remove duplicated rows
cobfinalded <- coberturafinal[!duplicated(coberturafinal), ]
# todo este trecho acima tem de ser recalculado
#
#Esta parte dos graficos aqui para baixo esta OK
#for ggplot need to use function grid.arrange
#https://stackoverflow.com/questions/1249548/side-by-side-plots-with-ggplot2
require(gridExtra)
coberturafinal$tipocob <- factor(coberturafinal$tipocob, levels = c("cobtotal", "cobmm"))

plot1=ggplot(coberturafinal, aes(x=ano2,y=percob, fill = tipocob))+ geom_boxplot()
plot2=ggplot(coberturafinal, aes(x=ano2,y=percob, fill = tipocob))+ geom_boxplot()
plot3=ggplot(coberturafinal, aes(x=ano2,y=percob, fill = tipocob))+ geom_boxplot()
plot4=ggplot(coberturafinal, aes(x=ano2,y=percob, fill = tipocob))+ geom_boxplot()

plot1
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

#fazendo a figura 2 como barplot com media e desvio padrao
# Summarize data to get mean and SD
library(dplyr)
library(ggplot2)
summary_data <- coberturafinal %>%
  group_by(trat,ano2,tipocob) %>%
  summarize(
    mean_value = mean(percob),
    sd_value = sd(percob)
  )

# Create the barplot with error bars fig 2
# treatment 1 plot
summary_data1 <- subset(summary_data, trat == 1)

#ggplot(summary_data1, aes(x = ano2, y = mean_value)) +
  geom_col(fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value), width = 0.2) +
  labs(x = "Year", y = "Mean Value") +  # Add labels
  theme_bw()  # Optional: Add a clean theme

#correct barplot https://towardsdatascience.com/grouped-barplot-with-error-bars-in-r-ee87b112204d/
ggplot(summary_data1,  aes(x = ano2, y = mean_value, fill = tipocob))+
  geom_col( position = "dodge", width = 0.5, alpha = 0.5, color = "black", size = 0.1) + geom_errorbar(aes(ymin = mean_value-sd_value, ymax = mean_value+sd_value),
                                                                                                       position =  position_dodge(width = 0.5), width = 0.2)
#BARPLOT WITH ERROR BARS FIGURE 6 FINAL
#calculate mean values of y
library(dplyr)
library(ggplot2)
fig6data = as.data.frame(data_fig_6_28apr2025)
fig6data$trat = as.factor(fig6data$trat)
fig6data$year = as.factor(fig6data$year)
summary_data6 <- fig6data %>%
  group_by(trat,year) %>%
  summarize(
    number_of_trays = mean(trays),
    sd_value = sd(trays)
  )
#Now do the plots FIGURE 6 FINAL
conc1 <- subset(summary_data6, trat == 1)
conc2 <- subset(summary_data6, trat == 2)
conc3 <- subset(summary_data6, trat == 3)
conc4 <- subset(summary_data6, trat == 4)
cplot1 =ggplot(conc1,  aes(x = year, y = number_of_trays, fill = year))+
  geom_col( position = "dodge", width = 0.5, alpha = 0.5, color = "black", size = 0.1) + geom_errorbar(aes(ymin = number_of_trays-sd_value, ymax = number_of_trays+sd_value),
                                                                                                       position =  position_dodge(width = 0.5), width = 0.2)  + ggtitle("A") + labs( x = "Year", y = "Number of trays")                                                                                               
cplot2 =ggplot(conc2,  aes(x = year, y = number_of_trays, fill=year))+
  geom_col( position = "dodge", width = 0.5, alpha = 0.5, color = "black", size = 0.1) + geom_errorbar(aes(ymin = number_of_trays-sd_value, ymax = number_of_trays+sd_value),
                                                                                                       position =  position_dodge(width = 0.5), width = 0.2)   + ggtitle("B") + labs( x = "Year", y = "Number of trays")                                                                                             
cplot3 =ggplot(conc3,  aes(x = year, y = number_of_trays, fill=year))+
  geom_col( position = "dodge", width = 0.5, alpha = 0.5, color = "black", size = 0.1) + geom_errorbar(aes(ymin = number_of_trays-sd_value, ymax = number_of_trays+sd_value),
                                                                                                       position =  position_dodge(width = 0.5), width = 0.2)  + ggtitle("C") + labs( x = "Year", y = "Number of trays")                                                                                              
cplot4 =ggplot(conc4,  aes(x = year, y = number_of_trays, fill=year))+
  geom_col( position = "dodge", width = 0.5, alpha = 0.5, color = "black", size = 0.1) + geom_errorbar(aes(ymin = number_of_trays-sd_value, ymax = number_of_trays+sd_value),
                                                                                                       position =  position_dodge(width = 0.5), width = 0.2)   + ggtitle("D") + labs( x = "Year", y = "Number of trays")                                                                                               
grid.arrange(cplot1, cplot2, cplot3, cplot4, ncol=2)

