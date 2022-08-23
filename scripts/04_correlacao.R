#carregando os pacotes necessários

#install.packages("descr")
#install.packages("summarytools")
#install.packages("reshape2")
#install.packages("tidyverse")

library(summarytools)
library(descr)
library(reshape2)
library(tidyverse)

#Correlação

##A variável x tem alguma relação com a variável y?
##Mensura a "força" da relação ou associação entre duas variáveis contínuas x e y. 

##Coeficiente de Correlação de Pearson
###Covariância: o quanto uma variável se modifica quando a outra se modifica

##simulando um data.frame de idade e níveis séricos de triglicerídeos
trig = data.frame(
  idade = sort(rnorm(26, 50, 4)),
  nivser_trig = sort(rnorm(26, 170, 15))
)

####gráfico de dispersão
gra01 = ggplot(trig) 

gra01 + 
  geom_point(aes(x = idade, y = nivser_trig), colour = "blue")+
  xlab("Idade")+ylab("Níveis séricos de triglicerídeos")+
  theme_bw()

####calculando a covariância: o quanto duas variáveis "variam conjuntamente"
#####Pode ser calculado com diferentes unidades de medida
#####Covariância = 0 -> não há aumento ou descréscimo em uma variável com base no aumento ou decréssimo da outra
cov(trig$idade, trig$nivser_trig)

####calculando coeficiente de correlação de Pearson
#####Representada por valores entre -1 e 1, em que 0 representa ausência de correlação
#####Quanto mais próximo de 1, maior a força de correlação positiva e quanto mais 
#####próximo de -1 maior a força de associação negativa
cor(trig$idade, trig$nivser_trig, method = "pearson")

##simulando um data.frame de idade e altura
alt = data.frame(
  idade = sort(rnorm(26, 50, 4)),
  altura = runif(26, 1.75, 1.82)
)

####gráfico de dispersão
gra02 = ggplot(alt) 

gra02 + 
  geom_point(aes(x = idade, y = altura))+
  xlab("Idade")+ylab("Altura")+
  theme_bw()

####calculando a covariância
cov(alt$idade, alt$altura)

####calculando coeficiente de correlação de Pearson
cor(alt$idade, alt$altura, method = "pearson")

##simulando um data.frame de tempo de espera em fila e satisfação do cliente
esp = data.frame(
  min_espera = sort(rnorm(50, 30, 20)),
  satisfacao = sort(runif(50, 0, 10), decreasing = T)
)

####gráfico de dispersão
gra03 = ggplot(esp) 

gra03 + 
  geom_point(aes(x = min_espera, y = satisfacao), colour = "red")+
  xlab("Tempo de espera em minutos")+ylab("Satisfação do cliente")+
  theme_bw()

####calculando a covariância
cov(esp$min_espera, esp$satisfacao)

####calculando coeficiente de correlação de Pearson
cor(esp$min_espera, esp$satisfacao, method = "pearson")

####simulando um data.frame de qualidade de vida por idade - relação não linear
quali = data.frame(
  idade = c(sort(runif(40, 20, 39)), sort(runif(40, 40, 59)), sort(runif(40, 60, 79))),
  qualidade_de_vida =  c(sort(rnorm(40, 70, 10)), rnorm(40, 95, 5), sort(rnorm(40, 70, 10), decreasing = T))
      )

####gráfico de dispersão
gra04 = ggplot(quali) 

gra04 + 
  geom_point(aes(x = idade, y = qualidade_de_vida), colour = "darkgreen")+
  xlab("Idade")+ylab("Qualidade de vida")+
  theme_bw()

####calculando a covariância
cov(quali$idade, quali$qualidade_de_vida)

####calculando coeficiente de correlação de Pearson
cor(quali$idade, quali$qualidade_de_vida, method = "pearson")

##Coefiênte de rho de Spearman
###Os cálculos envolven uma transformação prévia dos dados
###Valores postos em uma sequencia ordenada

##simulando um data.frame com variáveis x e y arbitrárias
obs= data.frame(
  individuo = 1:12,
  x = c(85, 71, 77, 67, 91, 72, 79, 92, 69, 71, 88, 85),
  y = c(83, 76, 77, 69, 96, 65, 68, 91, 64, 80, 91, 71)
)

####gráfico de dispersão
gra05 = ggplot(obs) 

gra05 + 
  geom_point(aes(x = x, y = x), colour = "magenta")+
  xlab("Variável x")+ylab("Variável y")+
  theme_bw()

####correlação de Spearman
cor(obs$x, obs$y, method = "spearman")

####correlação de Pearson
cor(obs$x, obs$y, method = "pearson")

####calculando coeficiente de correlação de Pearson
cor(quali$idade, quali$qualidade_de_vida, method = "spearman")

#simulando um data.frame com atendimentos por hora em um estabelecimento de saúde
atend = data.frame(
  hora = sort(round(runif(100, 6, 18),2)),
  atendimentos =  sort(round(rexp(100, 0.2)*10))
)

####gráfico de dispersão
gra06 = ggplot(atend) 

gra06 + 
  geom_point(aes(x = hora, y = atendimentos), colour = "purple")+
  xlab("Hora")+ylab("Número de atendimentos")+
  theme_bw()

####correlação de Pearson
cor(atend$hora, atend$atendimentos, method = "pearson")

####calculando coeficiente de correlação de Spearman
cor(atend$hora, atend$atendimentos, method = "spearman")
