#------------------------------------------------------------------------------#

# Carregar pacotes necessários

library(ggplot2)
library(dplyr)
library(readxl)

# Lendo a planilha

dados <- read_excel("Planilha geral_Carlos.xlsx")

head(dados) # Mostrar 5 primeiras linhas
summary(dados) # Resumir os dados
str(dados)

#------------------------------------------------------------------------------#

# Filtrar Positivo e Negativo

dados_flu <- dados %>%
  filter(
    `PCR Flu (controle)` %in% c("Positivo", "Negativo"),
    Táxon != "-"
  )

head(dados_flu) # Mostrar 5 primeiras linhas
summary(dados_flu) # Resumir os dados
str(dados_flu)

#------------------------------------------------------------------------------#

dados_flu$Táxon <- as.factor(dados_flu$Táxon)

str(dados_flu)

#------------------------------------------------------------------------------#

dados_flu %>%
  count(Táxon) %>%
  ggplot(aes(x = reorder(Táxon, -n), y = n)) +
  geom_bar(
    stat = "identity",
    fill = "black"
  ) +
  geom_text(
    aes(label = n),
    vjust = -0.3,
    size = 6
  ) +
  labs(
    x = "Táxon",
    y = "Número de indivíduos",
    title = "Contagem de indivíduos por Táxon"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1))
  ) +
  theme_classic(base_size = 18) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#------------------------------------------------------------------------------#