#carregando os pacotes necessários

#install.packages("descr")
#install.packages("summarytools")
#install.packages("reshape2")
#install.packages("tidyverse")

library(summarytools)
library(descr)
library(reshape2)
library(ggplot2)

# Estatísticas descritivas: variáveis e apresentação de dados

## Variáveis

### Variáveis numéricas no R

#### Para declarar um valor como inteiro no r, você deve usar a função as.integer
disc1 = as.integer(8)
disc2 = as.integer(c(8, 9, 10))

class(disc1)
class(disc2)

10 %/% 3 #resultados inteiros de uma divisão
3 %/% 2

10 %% 3 #quociente da divisão
3 %% 2 

#### Para declarar um valor contínuo no R 
cont1 = 8.9
cont2 = as.numeric(100.8)
cont2 = as.double(9.8)

### Variáveis qualitativas no R 

#### Para atribuirmos uma variável nominal no R devemos usar aspas nas categorias e podemos usar o comando factor
nomi1 = c("Sim", "Não", "Não", "Sim", "Sim")
table(nomi1)
class(nomi1)

nomi2 = as.factor(c("Sim", "Não", "Não", "Sim", "Sim"))
table(nomi2)
class(nomi2)

#### Para atribuirmos uma variável ordinal no R devemos usar aspas nas categorias e usar o comando factor
ordi1 = c("Superior", "Médio", "Fundamental", "Superior", "Superior", "Médio", "Médio", "Fundamental", "Médio")
table(ordi1)
class(ordi1)

ordi2 = factor(ordi1, levels = c("Superior", "Médio", "Fundamental"))
table(ordi2)
class(ordi2)

### Exemplo - construindo um data.frame

exemplo = data.frame(
  n = 1:40,
  idade = sample(40:70, 40, replace = T),
  estado_civil = c(rep("casada", 13), rep("divorciada", 13), rep("solteira", 10), rep("viúva", 4)),
  tabagista = c(rep("sim", 23), rep("não", 17)),
  idade_1_filho = sample(18:35, 40, replace = T),
  peso = runif(40, 55.0, 90.0),
  altura = runif(40, 150.0, 180.0),
  estado_saude = c(rep("bom", 20), rep("regular", 10), rep("ruim", 10))
)

### Tabelas

#### Criando uma tabela de frequência

tab01 = data.frame(table(exemplo$estado_civil))
tab01$porcentagem = tab01$Freq/sum(tab01$Freq)*100

summarytools::freq(exemplo$estado_civil, order = "freq") #summarytools
summarytools::freq(exemplo$estado_civil, order = "freq", report.nas = F) 

descr::freq(exemplo$estado_civil) #descr
descr::freq(exemplo$estado_civil, user.missing = T)

#### Criando uma tabela de dupla entrada/ tabela cruzada/ tabela de contingência
table(exemplo$tabagista, exemplo$estado_saude)
prop.table(table(exemplo$tabagista, exemplo$estado_saude))

tab04 = data.frame(table(exemplo$tabagista, exemplo$estado_saude))
colnames(tab04) = c("tabagista", "estado_saude", "n")
tab04 = dcast(tabagista ~ estado_saude, data = tab04)

descr::CrossTable(exemplo$tabagista, exemplo$estado_saude)
gmodels::CrossTable(exemplo$tabagista, exemplo$estado_saude)

#### Gráfico de dispersão - relação entre duas variáveis quantitativas contínuas

gra01 = ggplot(exemplo) #criando um objeto do tipo ggplot

gra01 + #criando um gráfico de dispersão simples
  geom_point(aes(x = altura, y = peso))

gra01 + #personalizando o gráfico de dispersão
  geom_point(aes(x = altura, y = peso), colour = "red")+
  xlab("Altura")+ylab("Peso")+
  ggtitle("Dispersão do peso por altura")+
  theme_classic()

### Gráfico de hastes ou bastões - frequência de uma variável quantitativa discreta

gravidez = data.frame(
  numero = c(0, 1, 2, 3, 4, 5, 6, 7, 8),
  frequencia = c(3, 6, 9, 13, 5, 2, 1, 0, 1)
)

gra02 = ggplot(gravidez, aes(x = numero, y = frequencia))

gra02 +
  geom_segment(aes(x = numero, xend = numero, y = 0, yend = frequencia))+
  geom_point(size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2)+
  xlab("Número de gravidezes")+ylab("Frequências absolutas")+
  theme_bw()

### Gráfico de barras - distribuição de frequencias de uma variável qualitativa

gra03 = data.frame(table(exemplo$estado_civil))
colnames(gra03) = c("estado_civil", "n")
gra03 = ggplot(gra03, aes(x = estado_civil, y = n))

gra03+
  geom_bar(stat = "identity", fill = "darkgreen")+ 
  xlab("Estado cívil")+ylab("Frequência")+
  theme_bw()+ geom_text(aes(label = n), vjust = -0.2)+
  scale_y_continuous(breaks=seq(0, 15, 1))

### Gráfico de linhas - variável contínua, em especial séries temporais

tempo = data.frame(
  mes = 1:12,
  taxa = c(20.6, 88.7, 237.9, 311.8, 162.0, 26.8, 13.7, 4.2, 3.2, 2.4, 4.5, 8.3)
)

gra04 = ggplot(tempo, aes(x = mes, y = taxa))

gra04 + 
  geom_line(colour = "blue")+
  #geom_point(size = 2, colour = "darkblue")+
  theme_light() +
  ggtitle("Taxa de incidência de dengue notificados em SP, 2010")+
  xlab("Mês")+ylab("Taxa de incidência")+
  scale_x_continuous(breaks=seq(0, 12, 1))+
  scale_y_continuous(breaks=seq(0, 450.0, 50.0))+
  theme_bw()
  
### Gráfico de setores - variável qualitativa, em especial nominal

library(RColorBrewer)
colors <- brewer.pal(5, "Set2") 

pie(exemplo$estado_civil, labels = exemplo$estado_civil, border = "white", col = colors)
