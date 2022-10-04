#carregando os pacotes necessários

#install.packages("descr")
#install.packages("summarytools")
#install.packages("reshape2")
#install.packages("tidyverse")
#install.packages("beeswarm")

library(summarytools)
library(descr)
library(reshape2)
library(tidyverse)
library(beeswarm)

# Estatísticas descritivas: medidas resumo

# Nesse desafio, você deverá usar o que aprendeu no encontro sobre medidas resumo
# Para isso, você deverá importar os dados do SIH registrados no DF em dezembro de 2020

sih = read_csv("dados/sih_df_dezembro_2020.csv")
colnames(sih) = c("IDADE", "SEXO", "UF_ZI", "MORTE", "DIAS_PERM")

## 01 - Calcule a média dos dias de permanência das pessoas que morreram e que não morreram

sih$MORTE = ifelse(sih$MORTE == 1, "MORTE", "SOBREVIVENCIA")
media = aggregate(DIAS_PERM ~ MORTE, data = sih, FUN = mean)

## 02 - Calcule o desvio padrão da idade das pessoas do sexo feminino e masculino

sih$SEXO = ifelse(sih$SEXO == 1, "MASCULINO", "FEMININO")
dev_pad = aggregate(IDADE ~ SEXO, data = sih, FUN = sd)

## 03 - Calcule a variância dos dias de permanência de maiores de 18 anos (pessoas que tem 19 anos ou mais)

sih_mais18 = subset(sih, sih$IDADE > 18)
var = round(var(sih_mais18$DIAS_PERM), 2)

## 04 - Compare as variabilidades do tempo de permanência e dos dias de permanência da população maior de 18 anos.

cv_idade = round((sd(sih_mais18$IDADE)/mean(sih_mais18$IDADE))*100, 2)
cv_perm = round((sd(sih_mais18$DIAS_PERM)/mean(sih_mais18$DIAS_PERM))*100, 2)

cv_perm/cv_idade
cv_idade/cv_perm

## Calcule o intervalo interquartílico da idade de homens e mulheres

h = subset(sih, sih$SEXO == "MASCULINO")
IQR(h$DIAS_PERM)

m = subset(sih, sih$SEXO == "FEMININO")
IQR(m$DIAS_PERM)
