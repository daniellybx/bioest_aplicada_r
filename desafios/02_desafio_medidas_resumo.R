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

# Nesse desafio, você deverá usar o que aprendeu no encontro sobre medidas resumo. Para responder as questões a seguir
# Para isso, você deverá importar os dados do SIH registrados no DF em dezembro de 2020

sih = read_csv("dados/sih_df_dezembro_2020.csv")
colnames(sih) = c("IDADE", "SEXO", "UF_ZI", "MORTE", "DIAS_PERM")

