#------------------------------------------------------------------------------#

# Carregar pacotes necessários

library(ggplot2)
library(dplyr)
library(tidyr)
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

  # Total de carcaças analisadas
total <- nrow(dados_flu)

# Contagem total por táxon
total_taxon <- dados_flu %>%
  count(Táxon) %>%
  rename(Total = n)

# Contagem de positivos por táxon
positivo_taxon <- dados_flu %>%
  filter(`PCR Flu (controle)` == "Positivo") %>%
  count(Táxon) %>%
  rename(Positivo = n)

# Preparar dados para o gráfico
dados_plot <- total_taxon %>%
  left_join(positivo_taxon, by = "Táxon") %>%
  mutate(Positivo = ifelse(is.na(Positivo), 0, Positivo)) %>%
  pivot_longer(
    cols = c(Total, Positivo),
    names_to = "Grupo",
    values_to = "Contagem"
  ) %>%
  mutate(
    Grupo = factor(
      Grupo,
      levels = c("Total", "Positivo")
    )
  )

# Gráfico
ggplot(
  dados_plot,
  aes(
    x = reorder(Táxon, -Contagem),
    y = Contagem,
    fill = Grupo
  )
) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.8)
  ) +
  geom_text(
    aes(label = Contagem),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 6
  ) +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste("Total carcaças de MM recolhidas =", total),
    hjust = 1.1,
    vjust = 1.5,
    size = 6,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Total" = "black",
      "Positivo" = "gray70"
    ),
    breaks = c("Total", "Positivo")
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = "Táxon",
    y = "Número de indivíduos",
    title = "Contagem de indivíduos por Táxon",
    fill = ""
  ) +
  theme_classic(base_size = 18) +
  theme(
    plot.title = element_text(
      size = 20,
      face = "bold",
      hjust = 0.5
    ),
    axis.title = element_text(
      size = 18,
      face = "bold"
    ),
    axis.text = element_text(
      size = 16
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 16)
  )
