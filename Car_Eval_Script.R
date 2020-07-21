#Projeto de SAD - Avaliação de Carros
#Realizado por: Bruno Saraiva 20160782; Diogo Palos 30001058; Miguel Nunes 30000814; Ricardo Melo 30000486

#Criação de Bibliotecas
install.packages("dplyr")
library(dplyr)

#Compreensão do Negócio
  #Importar Dataset
   car <- read.csv("car.data")
   
   #Mostra a dataset
  View(car)

#Compreensão de Dados
  #Mostra cada tipo de variável
  class(car$buying)
  class(car$maint)
  class(car$doors)
  class(car$persons)
  class(car$lug_boot)
  class(car$safety)

  #Conversão dos dados para númericos
  gsub("low","1",car) #aplicar para low,med,high e vhigh
  
  #Análise anatalitica dos dados
  summary(car)
  
  
  
#Modelação
  
 #Modelo de Regressão Linear Simples
   #Criação do Gráfico Plot, variavel x=buying, y=maint
   plot(buying~maint, data=car)
   
   #Modelo de regressão simples 
   Model1 <- lm(buying ~ maint, data = car)
   
   #Linha de adequação do modelo 
   abline(Model1, col="red")
   
   print(Model1)
   
   #Cria os Plots e o rm(Model1) permite navegar entre eles
   plot(Model1)
   rm(Model)

   #Mostra um plot que relaciona a relação entre os dados parciais da manunteção com os concentrado
   termplot(Model1)
   
   #Sumarização do Modelo de Regressão Linear
   summary(Model1)
   
   #Mostra os residuais, quanto maior o numero mais afastado esta da reta
   Model1$residuals
  
   #Mostra os ajustados
   Model1$fitted.values
  
 #Modelo de Regressão Linear Multipla
   Model2= lm(buying ~ maint + persons + safety, data = car)
   
   #Mostra os Coeficientes
   print(Model2)
   
   #Sumariza a Informação
   summary(Model2)
   
   #Sumariza a Informação em RSquared
   summary(Model2)$r.squared
   
   #Sumariza a Informação em RSquared Ajustado
   summary(Model2)$adj.r.squared
   
   #Previsão de Comprar um Carro preço de manutenção med, pessoas max=2 e segurança low
   predict(Model2, data.frame(maint = 2, persons = 2, safety = 1))
   
#Testes e Avaliação
   
   #análise analitica de determinados dados
   summary(car[c("buying","maint","persons","safety")])
   
   #Variância e desvio padrão
   var(car$maint)
   sd(car$maint)
   
   #Verficar valor que se repete mais vezes 
   tail(names(sort(table(car$buying))),1)
   
   #Criar gráfico boxplot
   boxplot(car$buying, car$maint, main="Comparação custo do automóvel e custo manutenção", col="#58D3F7",
   ylab = "Custo de 1 a 4", names=c("Custo do automóvel", "Custo da manutenção"))
   
   #Criar histograma
   hist(car$safety, breaks=2, main="Histograma sobre a segurança", xlab="Escala de 1 a 3", ylab="Frequencia", 
   col= c("blue", "red"))
