#Projeto de SAD - Avalia��o de Carros
#Realizado por: Bruno Saraiva 20160782; Diogo Palos 30001058; Miguel Nunes 30000814; Ricardo Melo 30000486

#Cria��o de Bibliotecas
install.packages("dplyr")
library(dplyr)

#Compreens�o do Neg�cio
  #Importar Dataset
   car <- read.csv("car.data")
   
   #Mostra a dataset
  View(car)

#Compreens�o de Dados
  #Mostra cada tipo de vari�vel
  class(car$buying)
  class(car$maint)
  class(car$doors)
  class(car$persons)
  class(car$lug_boot)
  class(car$safety)

  #Convers�o dos dados para n�mericos
  gsub("low","1",car) #aplicar para low,med,high e vhigh
  
  #An�lise anatalitica dos dados
  summary(car)
  
  
  
#Modela��o
  
 #Modelo de Regress�o Linear Simples
   #Cria��o do Gr�fico Plot, variavel x=buying, y=maint
   plot(buying~maint, data=car)
   
   #Modelo de regress�o simples 
   Model1 <- lm(buying ~ maint, data = car)
   
   #Linha de adequa��o do modelo 
   abline(Model1, col="red")
   
   print(Model1)
   
   #Cria os Plots e o rm(Model1) permite navegar entre eles
   plot(Model1)
   rm(Model)

   #Mostra um plot que relaciona a rela��o entre os dados parciais da manunte��o com os concentrado
   termplot(Model1)
   
   #Sumariza��o do Modelo de Regress�o Linear
   summary(Model1)
   
   #Mostra os residuais, quanto maior o numero mais afastado esta da reta
   Model1$residuals
  
   #Mostra os ajustados
   Model1$fitted.values
  
 #Modelo de Regress�o Linear Multipla
   Model2= lm(buying ~ maint + persons + safety, data = car)
   
   #Mostra os Coeficientes
   print(Model2)
   
   #Sumariza a Informa��o
   summary(Model2)
   
   #Sumariza a Informa��o em RSquared
   summary(Model2)$r.squared
   
   #Sumariza a Informa��o em RSquared Ajustado
   summary(Model2)$adj.r.squared
   
   #Previs�o de Comprar um Carro pre�o de manuten��o med, pessoas max=2 e seguran�a low
   predict(Model2, data.frame(maint = 2, persons = 2, safety = 1))
   
#Testes e Avalia��o
   
   #an�lise analitica de determinados dados
   summary(car[c("buying","maint","persons","safety")])
   
   #Vari�ncia e desvio padr�o
   var(car$maint)
   sd(car$maint)
   
   #Verficar valor que se repete mais vezes 
   tail(names(sort(table(car$buying))),1)
   
   #Criar gr�fico boxplot
   boxplot(car$buying, car$maint, main="Compara��o custo do autom�vel e custo manuten��o", col="#58D3F7",
   ylab = "Custo de 1 a 4", names=c("Custo do autom�vel", "Custo da manuten��o"))
   
   #Criar histograma
   hist(car$safety, breaks=2, main="Histograma sobre a seguran�a", xlab="Escala de 1 a 3", ylab="Frequencia", 
   col= c("blue", "red"))
