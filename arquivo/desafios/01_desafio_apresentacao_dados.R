#carregando os pacotes necessários

#install.packages("descr")
#install.packages("summarytools")
#install.packages("reshape2")
#install.packages("tidyverse")
#install.packages("htmlTable")

library(readr)
library(summarytools)
library(descr)
library(reshape2)
library(tidyverse)
library(htmlTable)

# Estatísticas descritivas: variáveis e apresentação de dados

# Nesse desafio, você deverá usar o que aprendeu no encontro sobre apresentação de dados
# Para isso, você deverá importar os dados do SIH registrados no DF em dezembro de 2020

sih = read_csv("dados/sih_df_dezembro_2020.csv")
colnames(sih) = c("IDADE", "SEXO", "UF_ZI", "MORTE", "DIAS_PERM")

## 01 - Identifique e transforme as variáveis segundo o tipo, caso seja necessário. 

### identifique os tipos de cada variável

class(sih$IDADE)        #variável númerica contínua
class(sih$SEXO)         #variável categórica nominal
class(sih$UF_ZI)        #variável categórica nominal
class(sih$MORTE)        #variável categórica nominal
class(sih$DIAS_PERM)    #variável numérica discreta

### transforme variáveis de acordo com o tipo

sih$SEXO = as.character(sih$SEXO)
sih$UF_ZI = as.character(sih$UF_ZI)
sih$MORTE = as.character(sih$MORTE)

## 02 - Faça uma tabela de frequencia do número de internações por UF

### separe a UF de acordo com os dois primeiros dígitos da variável UF_ZI

sih$UF = substring(sih$UF_ZI, 1, 2)

### crie um dataframe com a frequencia de ocorrência por UF

table_sihdf = data.frame(table(sih$UF))

## 03 - Faça uma tabela de proporção por morte arredondando os valores por 2 casas decimais

### crie a tabela de frequencia e proporção usando a variável morte e usando a função round para arredondar os valores

round(prop.table(table(sih$MORTE)), 2)

## 04 - Substitua os valores de sexo por suas labels ("Masculino" e "Feminino") e crie um gráfico de barras

### substitua o valor "1" por "Masculino" e o valor "3" por "Feminino"

sih$SEXO[sih$SEXO == "1"] = "Masculino"
sih$SEXO[sih$SEXO == "3"] = "Feminino"

### crie um data.frame da tabela de frequencia dos dados

sih_sexo = data.frame(table(sih$SEXO))

### crie um gráfico de barras usando ggplot2 

sih_grafico = ggplot(sih_sexo)

sih_grafico +
  geom_bar(aes(x = Var1, y = Freq), stat = "identity", fill = "magenta")+
  xlab("Sexo") + ylab("Frequencia") + theme_bw()

## 05 - crie um histograma verde, de bordas brancas, de idades com 10 quebras usando o Rbase

## identifique e remova outliers, se houver 

boxplot(sih$IDADE)

## crie o histograma usando o Rbase

hist(
  x = sih$IDADE,
  xlab = "Idade",
  ylab = "Internações",
  main = "Histograma de idades por internação, DF, dezembro de 2020",
  breaks = 10,
  col = "darkgreen", border = "white")

