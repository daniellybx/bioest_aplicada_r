#carregando os pacotes necessários

#install.packages("descr")
#install.packages("summarytools")
#install.packages("reshape2")
#install.packages("tidyverse")

library(summarytools)
library(descr)
library(reshape2)
library(tidyverse)

#Fundamentos de probabilidade
##Teoria da Probabilidade -> Girolamo Cardano: probabilidades usadas em jogos de dados

##Experimentos determinísticos e não determinísticos

###Experimento determinístico - tem sempre no mesmo resultado quando produzido nas mesmas condições

###Experimento não determinístico (aleatório) - apresenta resultados diferentes quando produzido nas mesmas condições

##Evento - resultados ou subconjunto de resultados de um experimento

##Espaço amostral - O conjunto (ômega) de todos os resultados possíveis de um experimento

###Exemplos: 

####Exemplo 1

omega = c(1, 2, 3, 4, 5, 6) #definindo nosso espaço amostral

a = omega[lapply(omega, "%%", 2) == 0] #observar um número par
b = omega[omega %% 2 == 1] #observar um número impar
c = omega[omega > 4] #observar um número maior que 4
d = omega[omega == 8] #observar o número 8 

####Exemplo 2

#####Lançamento de duas moedas

A = "cara" #eventos possíveis
B = "coroa"

omega = data.frame(evento_1 = c(A,A), #espaço amostral das possibilidades de resultado dos experimentos
                   evento_2 = c(A,B), 
                   evento_3 = c(B,B), 
                   evento_4 = c(B,A))

experimento = sample(colnames(omega), 1)

####Exemplo 3

#####Classificando o tipo sanguíneo de um aluno aleatório escolhido de forma aleatória 

omega = c("A", "B", "AB", "O") #espaço amostral 

####Exemplo 4

#####Classificando o status de tagagismo de um aluno escolhido de forma aleatória 

omega = c("fumante", "não fumante") #espaço amostral 

##União e intersecção de eventos

###União: todos os elemntos que estão em a ou b em um espaço amostral omega
 
###Intersecção: todos os elementos que estão em a e b, simultanemamente, em um espaço amostral omega

omega = c(1, 2, 3, 4, 5, 6) #espaço amostral

a = omega[lapply(omega, "%%", 2) == 0] #números pares

b = omega[omega >= 3] #números maiores ou iguais a 3

interseccao = intersect(a, b) #selecionando intersecção 
interseccao

uniao = c(a, b) #selecionando união dos dados
uniao

##Complemento de um evento

###Complemento de um evento: inclui todos os resultados que não fazem parte de determinado evento

omega = c(1, 2, 3, 4, 5, 6) #espaço amostral

a = omega[lapply(omega, "%%", 2) == 0] #números pares

complemento = setdiff(omega, a)

##Probabilidade

###Dentre um conjunto de possibilidades qual é a probabilidade de um evento ocorrer?

###1. Uma probabilidade é sempre maior ou igual a 0
###2. Uma probabilidade é sempre menor ou igual a 1 
###3. Um evento impossível tem probabilidade 0
###4. A probabilidade da não-ocorrência de um evento é sempre 1 - ocorrência do evento

#Exemplos: número de ocorrências de um evento dividido pelo número de elementos em seu espaço amostral

amostra = c(1, 2, 3, 4, 5, 6) #definindo nosso espaço amostral

a = amostra[lapply(amostra, "%%", 2) == 0] #observar um número par
length(a)/length(amostra) #probabilidade de encontrar um número par na amostra

b = amostra[amostra %% 2 == 1] #observar um número impar
length(b)/length(amostra) #probabilidade de encontrar um número impar na amostra

c = amostra[amostra > 4] #observar um número maior que 4
length(c)/length(amostra) #probabilidade de encontrar um número maior que 4 na amostra

d = amostra[amostra == 8] #observar o número 8 
length(d)/length(amostra) #probabilidade de encontrar um número 8 na amostra - evento impossível = subconjunto vazio


