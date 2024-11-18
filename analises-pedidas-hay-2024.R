consolidado = as.data.frame (consolidado_correto_chuva_sementes_cobertura)
consolidado$pcviaveis = (consolidado$Viaveis / consolidado$Total)*100
consolidado$anofactor = as.factor(consolidado$Ano)
consolidado$ambientefactor = as.factor(consolidado$Ambiente)
consolidado$coletafactor = as.factor(consolidado$Coleta)

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
poissonreg34 <- glm(formula = Viaveis ~ factor(ambfactor) + factor(anofactor), family = poisson, data = consol34)

#Comparar chuva sementes 3 4 transformar poisson
poissonreg34 <- glm(formula = Viaveis ~ factor(ambfactor) + factor(anofactor), family = poisson, data = consol34)

#Comparar percent viaveis 1 2 transformar arcoseno ou melhor fazer binomial
#arcoseno so funciona se os dados nao tiverem muitos zero ou 100

#Comparar  percent viaveis 3 4 transformar arcoseno ou melhor fazer binomial

#for percent viables use binomial regression with cbind for proportions

#analise percentagem viaveis dentro mesmo ano para cada tratamento
consol1 <- subset(consol12, Ambiente == 1)
consol2 <- subset(consol12, Ambiente == 2)
consol3 <- subset(consol34, Ambiente == 3)
consol4 <- subset(consol34, Ambiente == 4)
#boxplots percentagem viaveis dentro mesmo ano
ggplot(consol1, aes(x=coletafactor,y=pcviaveis, fill = anofactor))+ geom_boxplot()
ggplot(consol2, aes(x=coletafactor,y=pcviaveis, fill = anofactor))+ geom_boxplot()
ggplot(consol3, aes(x=coletafactor,y=pcviaveis, fill = anofactor))+ geom_boxplot()
ggplot(consol4, aes(x=coletafactor,y=pcviaveis, fill = anofactor))+ geom_boxplot()

#analise percentagem viaveis dentro mesmo ano para tratamentos 1 vs 2 e 3 vs 4

#analise numero bandejas com zero sementes ao longo do tempo







