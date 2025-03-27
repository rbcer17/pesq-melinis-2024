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
poissonreg1 <- glm(formula = Total ~ factor(coletafactor) + factor(anofactor), family = poisson, data = consol1)
summary(poissonreg1)
anova(poissonreg1)
# Dentro de cada tratamento comparar o numero de viaveis por coleta e ano 2 way


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
ggplot(consol12, aes(x=coletafactor,y=pcviaveis, fill = anofactor))+ geom_boxplot() + labs( x = "Collection", y = "Percentage of full caryopses", fill = "Year")

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

