#carregando os pacotes necessários

#install.packages("descr")
#install.packages("summarytools")
#install.packages("reshape2")
#install.packages("tidyverse")

library(summarytools)
library(descr)
library(reshape2)
library(tidyverse)

#Estatística descritiva: medidas-resumo

##Medidas-resumo: estatísticas descritivas que permitem caracterizar um conjunto de dados quantitativos de acordo
##com sua tendência central ou dispersão

###Notação
####variáveis estatísticas, geralmente, são representadas por letras maiúsculas e observações amostrais são sempre 
####representadas por letras minúsculas. "x" denota uma observação de "X", de modo que i varia de 1 a n

X = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100) #Variável numérica contínua com n = 6

x1 = X[1] #1ª observação
x3 = X[3] #3ª observação
xn = X[length(X)] #N observação

####A letra grega sigma (Σ) denota uma soma
sigma_X = sum(X)

##Medidas de posição ou de tendência central
Z = rnorm(1000, 100, 50) #gerand0 1000 números aleatórios em que a média é 100 e o desvio padrão é 50
hist(Z) #gerando um histograma da distribuição populacional

###Média: sintetiza em um único valor todas as nossas observações amostrais

#### Média aritimética
z_mean = round(mean(Z)) #média populacional

z_amostra = sample(Z, 100) #amostra da população Z
hist(z_amostra) #gerando um histograma da distribuição populacional

z_barra = round(mean(z_amostra)) #média amostral 

#### Para tirar a média de um grupo em que os dados estão agregados, desde que se conheça o n
grupo_alunos = data.frame(
  grupo = c("Portadores de sobrepeso", "Portadores de obesidade", "Portadores de peso adequado"),
  n = c(6, 14, 10), 
  x_barra = c(145.5, 148.8, 149.3)
)

grupo_alunos$soma = round(grupo_alunos$x_barra*grupo_alunos$n, 1)
mean_alunos = round(sum(grupo_alunos$soma)/sum(grupo_alunos$n), 1)

grupo_alunos = data.frame(
  grupo = c("Portadores de sobrepeso", "Portadores de obesidade", "Portadores de peso adequado"),
  n = c(600, 14, 10000), 
  x_barra = c(150, 170, 180)
)

grupo_alunos$soma = round(grupo_alunos$x_barra*grupo_alunos$n, 1)
mean_alunos = round(sum(grupo_alunos$soma)/sum(grupo_alunos$n), 1)

###Mediana: valor do meio da distribuição. Divide as observações em duas partes iguais.

#### Não é sensível a valores extremos
Z_median = median(Z)
z_median = median(z_amostra)

###Moda: valor que mais se repete em uma distribuição | não significa que será a maioria e pode não ser única
W = round(runif(100, min = 10, max = 20))
hist(W, breaks = 11)

stats_mode <- function(x) { #gerando uma função que calcula a moda
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

w_moda = stats_mode(W) #moda da população observada

W_table = data.frame(table(W)) #gerando uma tabela de frequencia dos valores
W_table[order(W_table$Freq, decreasing = T), ] #ordenando tabela do mais frequente para o menos frequente 

###Média geométrica 
####Função exponencial da média dos dados transformados | Muito útil em distribuições assimétricas
####Não pode ser usada se temos valores menores ou iguais a 0
R = c(2.2, 3.4, 3.9, 5.3, 57, 6.1, 6.5, 10.4, 15.6, 19.9, 42.8, 60.6, 70.2)

plot(R)
abline(h = mean(R), col="red",  lwd=3, lty=2)

r_log = log(R) #transformação logarítmica

plot(r_log)
abline(h = mean(r_log), col="blue",  lwd=3, lty=2)

mean_r_log = mean(r_log) #média da distribuição com os valores transformados

r_exp = exp(r_log) #transformação exponencial ("destransformando a distribuição")
plot(r_exp)
abline(h = mean(r_exp), col="green",  lwd=3, lty=2)

geom_mean = exp(mean_r_log)
abline(h = geom_mean, col="magenta",  lwd=3, lty=2)

##Medidas de dispersão
###Medem a variabilidade dos dados
amostra_A = c(149, 156, 157, 158, 159, 160, 161, 164)
amostra_B = c(132, 138, 152, 157, 160, 171, 176, 178)

mean(amostra_A)
mean(amostra_B)

median(amostra_A)
median(amostra_B)

plot(amostra_A)
plot(amostra_B)

###Amplitude amostral: maior diferença possível entre duas quaisquer observações da amostra
amp_A = max(amostra_A)-min(amostra_A)
amp_B = max(amostra_B)-min(amostra_B)

###Desvio médio: medida de dispersão das observações em torno da média amostral | média dos desvios dos valores absolutos
T = c(38, 40, 49, 67, 33, 57, 54, 64)
mean(T)
T_minus_mean = T - mean(T)
T_desv_med = mean(T_minus_mean)
T_desv_med = mean(abs(T_minus_mean)) #média do módulo dos desvios das observações em relação à média

###Variância: média do quadrado dos desvios
T_var = var(T)
T_var_manual = (sum((T_minus_mean)^2))/(length(T)-1)

###Desvio padrão: raiz quadrada da variância
####quanto maior o desvio padrão, maior a dispersão dos dados
T_sd = sd(T)
T_sd_manual = sqrt((sum((T_minus_mean)^2))/(length(T)-1))

###Coeficiente de variação: dado pela razão entre a média e o desvio padrão multiplicado por 100 | Expresso em porcentagem
####não possui unidade de medida 
T_CV = (sd(T)/mean(T))*100

####útil para comparar duas variabilidades 

cabras = data.frame(
  cabra = 1:10,
  peso = c(37.5, 46.6, 42.3, 39.4, 40.4, 47.4, 38.3, 39.9, 32.5, 35.9),
  comprimento = c(66.7, 74.3, 71.7, 62.9, 61.1, 69.5, 70.9, 72.1, 61.4, 73.4)
)

cv_peso = (sd(cabras$peso)/mean(cabras$peso))*100
cv_comp = (sd(cabras$comprimento)/mean(cabras$comprimento))*100

##Medidas descritivas e mudanças de escala
p_alt = c(160, 168, 159, 160, 153, 161, 157, 156, 150, 161)
p_mean = mean(p_alt)
p_sd = sd(p_alt)

p_alt_m = p_alt*0.01
p_mean_m = mean(p_alt_m)
p_mean*0.01 #transformando em metro

p_sd_cm = sd(p_alt_cm) #valores na escala de metros
p_sd*0.01 #transformando em metros

##Outras medidas-resumo importantes
###Quartis: divide a distribuição em 4 partes
V = rnorm(100000, mean= 100, sd = 10)

hist(V)

q1 = quantile(V, 0.25)
q2 = quantile(V, 0.50) #mesmo valor da mediana
q3 = quantile(V, 0.75)

hist(V, breaks = 100, col = "white")
abline(v = q1, col="red",  lwd=3, lty=2)
abline(v = q2, col="magenta",  lwd=3, lty=2)
abline(v = q3, col="blue",  lwd=3, lty=2)

###Intervalo interquartil: intervalo entre o primeiro e o terceiro quartis de uma distribuição
####representa a dispersão de 50% das observações próximas à mediana | quanto maior, maior a dispersão dos dados
####não é influenciada por valores extremos como a amplitude
IQR(V) 
q3 - q1

###Quantis:divide a distribuição em um determinado número de partes
#tercis: dividem em 3 partes
ter_v = quantile(V, 0.33)

#quintis: dividem em 5 partes
qui_v = quantile(V, 0.20)

#sextis: dividem em 6 partes
sex_v = quantile(V, 0.16)

#percentis: dividem em 100 partes
per_v = quantile(V, 0.01)

###Box plot: diagrama de caixa é um gráfico que descreve a distribuição dos dados por meio dos quartis
####útil para variáveis quantitativas | mínimo de 5 observações para sua construção
####retangulo = intervalo interquartil | altura = amplitude | 
M = rnorm(100, 10, 5)

boxplot(M)
stripchart(M,              
           method = "jitter", 
           pch = 19,          
           col = 4,           
           vertical = TRUE,   
           add = TRUE)        

####podemos representar mais de um boxplot no mesmo gráfico com ggplot2 | não podem ser variáveis diferentes
alunos = data.frame(
  classe = c(rep("A", 40), rep("B", 40), rep("C", 40), rep("D", 40)),
  altura = rnorm(160, 1.70, 0.15)
)

boxplot(altura ~ classe, data = alunos, col = c("green", "magenta", "blue", "red"), border = "darkgrey")
boxplot(altura ~ classe, data = alunos, col = "white")
stripchart(altura ~ classe, 
           data = alunos,
           method = "jitter", 
           pch = 19,          
           col = c("green", "magenta", "blue", "red"),           
           vertical = TRUE,   
           add = TRUE)   

###gráfico de valores individuais
####alternativas ao boxplot quando se tem poucar observações 
atletas = data.frame(
  pais = c(rep("Alemanha", 5), rep("Brasil", 5), rep("Canadá", 5), rep("Dinamarca", 5)),
  altura = rnorm(160, 1.70, 0.15)
)

beeswarm(altura ~ pais, 
         data = atletas, 
         col = c("#FFE099", "#009C3B", "#F76D5E", "#3FA0FF"))

stripchart(altura ~ pais, 
           data = atletas,
           method = "jitter", 
           pch = 19,          
           col = c("green", "magenta", "blue", "red"),           
           vertical = TRUE)
