library(tidyverse)
library(htmlTable)

#Criando uma tabela em formato de matrix
mx <- matrix(1:16, ncol = 2, nrow = 8)

rownames(mx) <- c("M", "F", "1-10", "11-20", "21-30", "31-40", "41-50", "50+")
colnames(mx) <- c("Frequencia", "%")

#Formatando a tabela em html com subgrupos em linhas
htmlTable(mx,
          rgroup = c("Sexo", "Faixa Etaria"),
          n.rgroup = c(2,4)
)

#Criando uma tabela em formato de matrix
mx2 <- matrix(1:48, ncol = 6, nrow = 8)

rownames(mx2) = paste0("Linha", 1:8)     
colnames(mx2) = paste0("Col", 1:6)  

#Formatando a tabela em html com subgrupos em colunas
htmlTable(mx2,
          cgroup = c("Sexo", "Faixa Etaria"),
          n.cgroup = c(2,4)
)

htmlTable(mx2,
          cgroup = rbind(c("", "Variaveis", NA),
                         c("", "Sexo", "Faixa Etaria")),
          n.cgroup = rbind(c(1,2,NA),
                           c(2,2,2))
)
