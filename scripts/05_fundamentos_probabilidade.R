#carregando os pacotes necessários

#install.packages("descr")
#install.packages("summarytools")
#install.packages("reshape2")
#install.packages("tidyverse")
#install.packages("VennDiagram")

library(summarytools)
library(descr)
library(reshape2)
library(tidyverse)
library(VennDiagram)

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

##Diagrama de Venn e cálculos de probabilidade

###Comunidade = 20% dos índivídus adultos são hipertensos, 40% são diabéticos e 15% são hipertensos e diabéticos

### - D: indivíduo escolhido é portador de diabetes
### - H: indivíduo escolhido é portador de hipertensão
### Intersecção de D e H é portador de diabetes e hipertensão


grid.newpage()
draw.pairwise.venn(area1=40, area2=20, cross.area=0.15,
                   category=c("Diabéticos","Hipertensos"),
                   fill=c("Red","Yellow"))


#### Qual a probabilidade de:
#### a. Ser diabético, mas não hipertenso?

a = 0.40 - 0.15

#### b. Ser hipertenso ou diabético (ou ambos)? {união dos eventos}

b = 0.25 + 0.15 + 0.05
b = 0.4 + 0.2 - 0.15

#### c. Não ser hipertenso nem diabético?

c = 1 - b
c = 1 - 0.25 - 0.15 - 0.05
c = 1 - (0.4 + 0.2 - 0.15)

## Probabilidade Condicional
### Probabilidade da ocorrência de um evento dado que outro evento já ocorreu

#### d. Qual a probabilidade de um índividuo ser portador de hipertensão, dado que ele possui diabetes?
d = 0.15/0.4

## Eventos mutuamente exclusivos
### Probabilidade da ocorrência de eventos que nunca ocorrem de forma simultânea

#### a -> indivíduos vegetarianos
#### b -> indivíduos fumantes

#### Qual a probabilidade dele ser vegetariano e fumante?
p1 = c()

#### Qual a probabilidade dele ser vegetariano ou fumante?
a = 0.3
b = 0.05

p2.1 = a + b # união - probabilidade de ser um ou outro

p2.2 = 1-p2.1 # complemento - probabilidade de não ser nenhum dos dois

grid.newpage() ##### intersecção entre A e B é um conjunto vazio
draw.pairwise.venn(area1=0.30, area2=0.05, cross.area=0,
                   category=c("Vegetarianos","Fumantes"),
                   fill=c("Green","Blue"))

## Eventos independentes
### Probabilidade da ocorrência de eventos que não tornam a ocorrência do outro mais ou menos provável

#### a -> doença cardiovascular
#### b -> depressão

#### A presença de A aumenta B e a presença de B aumenta A?
a = 0.15
b = 0.35

grid.newpage() ##### intersecção entre A e B é igual a 0.1
draw.pairwise.venn(area1=0.15, area2=0.35, cross.area=0.1,
                   category=c("Doença Cardiovascular","Depressão"),
                   fill=c("Red","Green"))

pintb = 0.1

p3.1 = round(pintb/b, 2) # prob de doença cardio é de 0.15, mas se a pessoa tem depressão aumenta para 0,29
p3.2 = round(pintb/a, 2) # prob de depressão é de 0.35, mas se a pessoa tem doença cardio aumenta para 0,67

# os eventos não são independentes, visto que as probabilidades aumentam na presença da combinação das duas condições

#### a -> diabetes
#### b -> depressão

#### A presença de A aumenta B e a presença de B aumenta A?
a = 0.15
b = 0.20

grid.newpage() ##### intersecção entre A e B é igual a 0.03
draw.pairwise.venn(area1=0.15, area2=0.20, cross.area=0.03,
                   category=c("Doença Cardiovascular","Depressão"),
                   fill=c("Pink","Yellow"))

pintb = 0.03

p4.1 = round(pintb/b, 2) # prob de diabetes é de 0.15 e se a pessoa tem depressão continua com 0.15
p4.2 = round(pintb/a, 2) # prob de depressão é de 0.20 e se a pessoa tem diabetes continua com 0.20

# os eventos são independentes, visto que as probabilidades da ocorrência dos eventos permanece inalterada mesmo combinando as duas condições

#### a -> ter um filho do sexo masculino
#### b -> ter um filho do sexo masculino

#### Qual a probabilidade de um casal ter dois filhos do sexo masculino?
a = 0.5
b = 0.5

grid.newpage() ##### intersecção entre A e B é igual a 0
draw.pairwise.venn(area1=0.5, area2=0.5, cross.area=0.0,
                   category=c("Filho do sexo masculino","Filho do sexo masculino"),
                   fill=c("lightblue","lightgreen"))

p5 = a*b

# os eventos são independentes e mutuamente exclusivos

##Odds

### A odds é uma forma de quantificar a chance de ocorrência de um evento e indica quantas vezes a 
### probabilidade de ocorrência de um evento equivale à probabilidade de ele não ocorrer

#### a = evento que ocorre em 80% das vezes

#### Qual a odds de um evento que ocorre em 80% das vezes? 4:1

o1 = 0.8/0.2 # probabilidade da ocorrência dividida pela probabilidade da não ocorrência (complemento)

#### a = evento que ocorre em 25% das vezes

#### Qual a odds de um evento que ocorre em 25% das vezes? 1:3

o2 = round(0.25/0.75, 2) # probabilidade da ocorrência dividida pela probabilidade da não ocorrência (complemento)

#### a = evento que ocorre em 50% das vezes

#### Qual a odds de um evento que ocorre em 50% das vezes? 1:1

o3 = 0.5/0.5 # probabilidade da ocorrência dividida pela probabilidade da não ocorrência (complemento)

##### Odds > 1: é mais provável que o evento ocorra do que não ocorra
##### Odds < 1: é mais provável que o não evento ocorra do que ocorra
##### Odds = 1: a probabilidade de ocorrência é igual a probabilidade de não ocorrência

## Exemplo: sensibilidade e especificidade de um teste diagnóstico

### T: o resultado do teste é positivo 
### D: o indivíduo é realmente portador da doença em questão 
### Tc: resultado negativo 
### Dc: o indivíduo não é portador da doença

### Sensibilidade(S) = P(T|D) #probabilidade do teste ser positivo dado que a pessoa tem a doença
### Especificidade(E) = P(Tc|Dc) #probabilidade do teste ser negativo dado que a pessoa não tem a doença

teste = matrix(c(20, 29, 7, 24), ncol = 2, nrow = 2, dimnames = list(c("Positivo", "Negativo"),
                                                                      c("Possui a Doença", "Não possui a doença")))

a = teste[1,1]
b = teste[1,2]
c = teste[2,1]
d = teste[2,2]

sensibilidade = round(a/(a+c), 3)
especificidade = round(d/(b+d), 3)