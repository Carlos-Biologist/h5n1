# 1) Pacotes
# install.packages(c("readxl", "dplyr"))  # rode uma vez, se ainda não tiver instalado
library(readxl)
library(dplyr)

# ---------------------------------------------------------------------------- #

# 2) Carregar a planilha
# Se o arquivo estiver na pasta de trabalho atual:
dados <- read_excel("Dados_Histopatologia.xlsx")

# Conferir a estrutura
str(dados)

# Ver os nomes das colunas
names(dados)

# ---------------------------------------------------------------------------- #

# 3) Número de amostras (IDs únicos)
n_amostras <- n_distinct(dados$ID, na.rm = TRUE)
n_amostras

# ---------------------------------------------------------------------------- #

