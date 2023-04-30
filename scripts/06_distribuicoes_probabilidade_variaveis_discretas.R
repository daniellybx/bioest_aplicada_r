#carregando os pacotes necessários

#install.packages("tidyverse")

library(tidyverse)

#Distribuições teóricas de probabilidade envolvendo variáveis discretas

#-> Se uma variável pode assumir uma série de valores diferentes tal que qualquer resultado
#seja determinado pelo "acaso", ela é uma *variável aleatória*

##Exemplo 1
## Variável número de filhos (X). Suponha que em uma dada população de mulheres adultas, 33% não possuem filhos
## , 25% possuem 1 filho, 20% possuem 2 filhos, 12% possuem três filhos, 7% possuem 4 filhos e 3% possuem 5 filhos. 
## Dados que nenhuma mulher dessa população possui 6 filhos temos que os possíveis resultados para a variável X
## são os eventos {X = 0},  {X = 1},  {X = 2},  {X = 3},  {X = 4} e {X = 5}. 
## Com as frequencias relativas, conseguimos calcular as probabilidades da ocorrência de eventos.
## Qual é a probabilidade de escolhermos ao acaso uma mulher adulta sem filhos P({X = 0})?

p_sem_filhos = 33/100

## Podemos fazer um conjunto com todas as probabilidades

p_1_filho = 25/100
p_2_filhos = 20/100
p_3_filhos = 12/100
p_4_filhos = 7/100
p_5_filhos = 3/100

## Podemos calcular a soma

p_total = p_sem_filhos + p_1_filho + p_2_filhos + p_3_filhos + p_4_filhos + p_5_filhos 

## Podemos criar um gráfico com esses dados ilustrando a probabilidadaede do número de filhos -> P(X = x)

p_df = data.frame(
  n_filhos = c(0, 1, 2, 3, 4, 5),
  prob = c(p_sem_filhos, p_1_filho, p_2_filhos, p_3_filhos, p_4_filhos, p_5_filhos)
)

gra01 = ggplot(p_df, aes(x = n_filhos, y = prob))
gra01 +
  geom_segment(aes(x = n_filhos, xend = n_filhos, y = 0, yend = prob))+
  geom_point(size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2)+
  xlab("Número de filhos")+ylab("Probabilidade")+
  theme_bw()

## Qual a probabilidade de escolhermos uma mulher com menos de três fihos?

#-> P(X<3) = P(X = 0 ou X = 1 ou X = 2)
#-> P(X<3) = P(X = 0) + (X = 1) + (X = 2)

p_menos_de_3 = p_sem_filhos + p_1_filho + p_2_filhos

## Qual a probabilidade de escolhermos uma mulher com até três fihos?

#-> P(X<=3) = P(X = 0 ou X = 1 ou X = 2 ou X = 3)
#-> P(X<=3) = P(X = 0) + (X = 1) + (X = 2) + (X = 3)

p_ate_de_3 = p_sem_filhos + p_1_filho + p_2_filhos + p_3_filhos

#Média de uma variável aleatória discreta

## A média de uma variável aleatória discreta é o somatório de todas as possibilidades do produto xi multiplicado por
## sua probabilidade -> ∑xi * P(X = xi) -> Chamamos isso de esperança E(X).

## Usando o exemplo anterior, vamos calcular a esperança(média) de filhos para aquela população E(X)

e_filhos = 0 * p_sem_filhos + 1 * p_1_filho + 2 * p_2_filhos + 3 * p_3_filhos + 4 * p_4_filhos + 5 * p_5_filhos 

#Desvio padrão de uma variável aleatória discreta

## O desvio padrão de uma variável aleatória depende de sua variância que é dada por Var(X) = ∑[xi-E(x)]^2 * P(X = xi), que 
## vira Var(X) = ∑[xi^2*P(X = xi)]-[E(X)]^2 

## Usando o exemplo anterior podemos calcular 

var_filhos = round((0^2 * p_sem_filhos + 1^2 * p_1_filho + 2^2 * p_2_filhos + 3^2 * p_3_filhos + 4^2 * p_4_filhos + 5^2 * p_5_filhos)-(1.44^2),2) 

## O desvio padrão é dado pela raiz quadrada da variância
dev_pad = round(sqrt(var_filhos), 2)

#Ensaio de Bernoulli
## Um ensaio de Bernoulli é um experimento aleatório cujo resultado é sempre classificado em duas categorias.
## É muito útil na epidemiologia para classificar morbidade, mortalidade e exposições. 

##Exemplo 2
## Espaço amostral de portadores de uma doença
espaco_amostral1 = c("possui a doença", "não possui a doença")
espaco_amostral1 = c(0, 1)

## Probabilidade de ser portador de uma doença P(X = 1) = p
p_portador = 0.5

## Probabilidade de não ser portador de uma doença P(X = 0) = 1 - p
p_nao_portador = 1 - p_portador

## Expressão de modo genérico P(X = x) = p^x*(1-p)^1-x

##Exemplo 3
## Considere uma taxa de prevalência de 1 caso em 100 habitantes. Aos escolhermos ao acaso um habitante dessa 
## população, qual a probabilidade de esse indivíduo ser portador da doença?

##P(X = 1)
p_doenca = 0.01^1*0.99^0

##P(X = 0)
p_ausencia = 0.01^0*0.99^1

#Distribuição Binomial
## Quando diversos experimentos de um ensaio de Bernoulli ocorrem, temos uma distribuição Binomial. 
## Ou seja, uma distribuição, com diversos valores 0 ou 1.

## P(Y=y) = (n y)*p^y*(1-p)^n-y

## Quais as características de uma variável aleatória que segue a distribuição binomial?
## a. estamos considerando n ensaios Bernoulli;
## b. esses ensaios são independentes entre si; e
## c. a probabilidade de sucesso(p) é igual em todos os ensaios

## No R podemos gerar valores aleatórios para uma distribuição binomial. Vamos gerar 1000 valores de uma distribuição binomial,
## com 10 ensaios, cuja probabilidade de sucesso é de 50%.
dist_binom = rbinom(n = 1000, size = 10, prob = 0.5)

## Podemos gerar o histograma dessa distribuição
hist(dist_binom, main = "Histograma de uma distribuição binomial", xlab = "Número de sucessos", ylab = "Frequência", col = "blue", border = "lightblue")

## Em uma plantação, a probabilidade de determinada semente germinar é de 80%.
germinar = 0.8
nao_germinar = 0.2

## Qual seria a probabilidade de duas sementes germinarem? 
duas_sementes_germinar = germinar*germinar

## Qual a probabilidade da primeira semente germinar e a segunda não germinar?
primeira_semente_germinar = germinar*nao_germinar

## Qual a probabilidade da primeira semente não germinar e a segunda germinar?
segunda_semente_germinar = nao_germinar*germinar

## Qual a probabilidade de nenhuma semente germinar?
nenhuma_semente_germinar = nao_germinar*nao_germinar

## E se plantarmos 3 sementes. Qual a probabilidade das três germinarem?
tres_sementes_germinar = germinar^3

## E se plantarmos 3 sementes. Qual a probabilidade de nehuma germinarem?
nenhuma_tres_sementes_germinar = (1-germinar)^3

## E se plantarmos 3 sementes. Qual a probabilidade de duas germinarem?
duas_de_tres_sementes_germinar = 3*germinar^2*(1-germinar)

## A média de uma v.a. que segue uma distribuição binomial é E(Y) = n*p
## Qual a média do exemplo anterior em um cenário em que 20 sementes foram plantadas?
n_sementes = 20
p_sementes = 0.8
e_media_vinte_sementes = n_sementes * p_sementes

### Quando se plantam 20 sementes, em média, 16 germinam. 

## Quais as funções do R para trabalhar com uma distribuição binomial?

### Função de probabilidade de massa (PMF) de uma distribuição binomial
###-> Qual a probabilidade de se obter exatamente 15 germinações em 20 sementes? 
pmf_15 = dbinom(x = 16, size = 20, prob = 0.8)

### Função de distribuição acumulada (CDF)
###-> Qual a probabilidade se obter até 15 germinações em 20 sementes?
cdf_15 = pbinom(q = 15, size = 20, prob = 0.8)

### Quantil de uma distribuição binomial
###-> Qual o número mínimo de sementes que devem germinar quando eu plano 100 sementes com uma probabilidade acumulada de 90%?
q_50 = qbinom(p = 0.9, size = 100, prob = 0.8)

#Distribuição de Poisson
## Muito usada para eventos raros, por isso é comum na epidemiologia. 
## Se n(ensaio) é um número muito grande e y(suceso) é um número muito pequeno, podemos usar a fórmula

## P(Y=y) = (e^-lambda*lambda^y)/y!

###- "e" é o Número Euleriano (~2.718282)
###- lambda é n*p (lambda deve ser um némero maior que 0)
###- y é um número inteiro maior ou igual a 0 

## Podemos gerar valores aleatórios para uma distribuição Poisson
dist_poisson = rpois(n = 500000, lambda = 5)

## Podemos gerar o histograma dessa distribuição
hist(dist_poisson, col = "blue", border = "lightblue", main = "Distribuição Poisson", xlab = "Valores", ylab = "Frequência")

##Exemplo 4
## Em uma cidade vivem 500.000 pessoas. Considere que a probabilidade de uma pessoa portar determinada doença é de 0.00001. 

## Qual a probabilidade de não encontrarmos nenhum portador da doença nessa cidade?
n = 500000
p = 0.00001

lambda = n*p

y = 0 

p_nenhum_portador = ((exp(1)^-lambda)*(lambda^y))/factorial(y)

## Qual a probabilidade de encontrarmos 10 portadores da doença nessa cidade?
n = 500000
p = 0.00001

lambda = n*p

y = 10 

p_10_portadores = ((exp(1)^-lambda)*(lambda^y))/factorial(y)

## A média e a variância de uma distribuição Poisson são iguais à lambda.
## E(Y) = Var(Y) = lambda

## Sendo assim, o desvio padrão de uma distribuição Poisson é a raiz quadrada de lambda

## Qual a esperança de casos da doença na cidade?
e_cidade = lambda

## Qual o desvio padrão dessa média na cidade?
dev_pad_cidade = sqrt(lambda)

## Quais as funções do R para trabalhar com uma distribuição Poisson?

### Densidade de probabilidade
###-> Qual a probabilidade de termos exatamente 10 casos de dengue em uma semana, dado que a taxa média de casos de dengue é de 7 casos por semana?
dens_poisson = dpois(x = 10, lambda = 7)

### Probabilidade acumulada
###-> Qual a probabilidade de observarmos no máximo 5 casos de dengue em uma seman, dado que a taxa média de casos de dengue é de 7 casos por semana?
acum_poisson = ppois(q = 5, lambda = 7)

### Quantil 
###-> Qual o menor número de casos de dengue que esperamos observar em uma semana dado que a probabilidade de observarmos no máximo 5 eventos é de 
### 0.3007083 e a taxa média de casos por semana é igual a 7?
qua_poisson = qpois(p = 0.3007083, lambda = 7)
