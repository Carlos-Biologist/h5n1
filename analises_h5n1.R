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
n_total <- nrow(dados_flu)

# Total de positivos Flu-A
n_positivo_fluA <- dados_flu %>%
  filter(`PCR Flu (controle)` == "Positivo") %>%
  nrow()

# Total de positivos H5
n_positivo_h5 <- dados_flu %>%
  filter(`PCR H5` == "Positivo") %>%
  nrow()

# Contagem total por táxon
total_taxon <- dados_flu %>%
  count(Táxon, name = "Total")

# Contagem de positivos Flu-A por táxon
positivo_fluA_taxon <- dados_flu %>%
  filter(`PCR Flu (controle)` == "Positivo") %>%
  count(Táxon, name = "Positivo_FluA")

# Contagem de positivos H5 por táxon
positivo_h5_taxon <- dados_flu %>%
  filter(`PCR H5` == "Positivo") %>%
  count(Táxon, name = "Positivo_H5")

# Juntar contagens
dados_plot_taxon <- total_taxon %>%
  left_join(positivo_fluA_taxon, by = "Táxon") %>%
  left_join(positivo_h5_taxon, by = "Táxon") %>%
  mutate(
    Positivo_FluA = ifelse(is.na(Positivo_FluA), 0, Positivo_FluA),
    Positivo_H5 = ifelse(is.na(Positivo_H5), 0, Positivo_H5)
  )

# Ordem dos táxons baseada no total
ordem_taxon <- dados_plot_taxon %>%
  arrange(desc(Total)) %>%
  pull(Táxon)

# Converter para formato longo
dados_plot_taxon <- dados_plot_taxon %>%
  pivot_longer(
    cols = c(Total, Positivo_FluA, Positivo_H5),
    names_to = "Grupo",
    values_to = "Contagem"
  ) %>%
  mutate(
    Grupo = factor(
      Grupo,
      levels = c("Total", "Positivo_FluA", "Positivo_H5")
    ),
    Táxon = factor(
      Táxon,
      levels = ordem_taxon
    )
  )

ggplot(
  dados_plot_taxon,
  aes(
    x = Táxon,
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
  scale_x_discrete(
    labels = c(
      "Pinípede" = "Pinípedes",
      "Cetáceo" = "Cetáceos",
      "Sirênio" = "Sirênios"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Total" = "black",
      "Positivo_FluA" = "gray70",
      "Positivo_H5" = "red"
    ),
    labels = c(
      paste0("Amostrados (n = ", n_total, ")"),
      paste0("Flu-A (n = ", n_positivo_fluA, ")"),
      paste0("H5 (n = ", n_positivo_h5, ")")
    ),
    breaks = c(
      "Total",
      "Positivo_FluA",
      "Positivo_H5"
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    x = "Táxons",
    y = "Número de indivíduos",
    title = "Número de indivíduos positivos para Influenza A e com subtipo H5",
    fill = NULL
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
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(
      size = 16,
      face = "bold"
    )
  )

# Salvar em alta resolução (opcional)
ggsave(
  "contagem_taxon.png",
  width = 14,
  height = 8,
  dpi = 600
)

#------------------------------------------------------------------------------#

# Totais para a legenda
n_total <- nrow(dados_flu)

n_positivo_fluA <- dados_flu %>%
  filter(`PCR Flu (controle)` == "Positivo") %>%
  nrow()

n_positivo_h5 <- dados_flu %>%
  filter(`PCR H5` == "Positivo") %>%
  nrow()

# Contagem total por espécie
total_especie <- dados_flu %>%
  count(Espécie, name = "Total")

# Contagem de positivos Flu-A por espécie
positivo_fluA_especie <- dados_flu %>%
  filter(`PCR Flu (controle)` == "Positivo") %>%
  count(Espécie, name = "Positivo_FluA")

# Contagem de positivos H5 por espécie
positivo_h5_especie <- dados_flu %>%
  filter(`PCR H5` == "Positivo") %>%
  count(Espécie, name = "Positivo_H5")

# Juntar contagens
dados_plot <- total_especie %>%
  left_join(positivo_fluA_especie, by = "Espécie") %>%
  left_join(positivo_h5_especie, by = "Espécie") %>%
  mutate(
    Positivo_FluA = ifelse(is.na(Positivo_FluA), 0, Positivo_FluA),
    Positivo_H5 = ifelse(is.na(Positivo_H5), 0, Positivo_H5)
  )

# Ordem das espécies baseada no total
ordem_especies <- dados_plot %>%
  arrange(desc(Total)) %>%
  pull(Espécie)

# Converter para formato longo
dados_plot <- dados_plot %>%
  pivot_longer(
    cols = c(Total, Positivo_FluA, Positivo_H5),
    names_to = "Grupo",
    values_to = "Contagem"
  ) %>%
  mutate(
    Grupo = factor(
      Grupo,
      levels = c("Total", "Positivo_FluA", "Positivo_H5")
    ),
    Espécie = factor(
      Espécie,
      levels = ordem_especies
    )
  )

# Gráfico
ggplot(
  dados_plot,
  aes(
    x = Espécie,
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
    size = 5
  ) +
  scale_x_discrete(
    labels = function(x) parse(text = paste0("italic('", x, "')"))
  ) +
  scale_fill_manual(
    values = c(
      "Total" = "black",
      "Positivo_FluA" = "gray70",
      "Positivo_H5" = "red"
    ),
    labels = c(
      paste0("Amostrados (n = ", n_total, ")"),
      paste0("Flu-A (n = ", n_positivo_fluA, ")"),
      paste0("H5 (n = ", n_positivo_h5, ")")
    ),
    breaks = c(
      "Total",
      "Positivo_FluA",
      "Positivo_H5"
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    x = "Espécie",
    y = "Número de indivíduos",
    title = "Número de indivíduos por espécie positivos para Influenza A e subtipo H5",
    fill = NULL
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
    axis.text.x = element_text(
      angle = 50,
      hjust = 1,
      vjust = 1,
      size = 12
    ),
    axis.text.y = element_text(
      size = 16
    ),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(
      size = 16,
      face = "bold"
    )
  )

# Salvar em alta resolução (opcional)
ggsave(
  "contagem_especies.png",
  width = 14,
  height = 8,
  dpi = 600
)

#------------------------------------------------------------------------------#

# Filtrar apenas pinípedes
dados_pinipedes <- dados_flu %>%
  filter(Táxon == "Pinípede")

# Totais para a legenda
n_total <- nrow(dados_pinipedes)

n_positivo_fluA <- dados_pinipedes %>%
  filter(`PCR Flu (controle)` == "Positivo") %>%
  nrow()

n_positivo_h5 <- dados_pinipedes %>%
  filter(`PCR H5` == "Positivo") %>%
  nrow()

# Contagens por espécie
total_especie <- dados_pinipedes %>%
  count(Espécie, name = "Total")

positivo_fluA_especie <- dados_pinipedes %>%
  filter(`PCR Flu (controle)` == "Positivo") %>%
  count(Espécie, name = "Positivo_FluA")

positivo_h5_especie <- dados_pinipedes %>%
  filter(`PCR H5` == "Positivo") %>%
  count(Espécie, name = "Positivo_H5")

# Juntar dados
dados_plot <- total_especie %>%
  left_join(positivo_fluA_especie, by = "Espécie") %>%
  left_join(positivo_h5_especie, by = "Espécie") %>%
  mutate(
    Positivo_FluA = replace_na(Positivo_FluA, 0),
    Positivo_H5 = replace_na(Positivo_H5, 0)
  )

# Ordem das espécies
ordem_especies <- dados_plot %>%
  arrange(desc(Total)) %>%
  pull(Espécie)

dados_plot <- dados_plot %>%
  pivot_longer(
    cols = c(Total, Positivo_FluA, Positivo_H5),
    names_to = "Grupo",
    values_to = "Contagem"
  ) %>%
  mutate(
    Grupo = factor(
      Grupo,
      levels = c("Total", "Positivo_FluA", "Positivo_H5")
    ),
    Espécie = factor(Espécie, levels = ordem_especies)
  )

# Gráfico
ggplot(
  dados_plot,
  aes(x = Espécie, y = Contagem, fill = Grupo)
) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.8)
  ) +
  geom_text(
    aes(label = Contagem),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 5
  ) +
  scale_x_discrete(
    labels = function(x) parse(text = paste0("italic('", x, "')"))
  ) +
  scale_fill_manual(
    values = c(
      "Total" = "black",
      "Positivo_FluA" = "gray70",
      "Positivo_H5" = "red"
    ),
    labels = c(
      paste0("Amostrados (n = ", n_total, ")"),
      paste0("Flu-A (n = ", n_positivo_fluA, ")"),
      paste0("H5 (n = ", n_positivo_h5, ")")
    )
  ) +
  labs(
    x = "Espécies",
    y = "Número de indivíduos",
    title = "Número de indivíduos por espécie de pinípedes positivos para Influenza A e com subtipo H5",
    fill = NULL
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.10))
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
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      size = 18
    ),
    axis.text.y = element_text(
      size = 16
    ),
    legend.position = "top",
    legend.text = element_text(
      size = 16,
      face = "bold"
    )
  )

ggsave(
  "Pinipedes_FluA_H5.png",
  width = 14,
  height = 8,
  dpi = 600
)

#------------------------------------------------------------------------------#

# Filtrar apenas cetáceos
dados_cetaceos <- dados_flu %>%
  filter(Táxon == "Cetáceo")

# Totais para a legenda
n_total <- nrow(dados_cetaceos)

n_positivo_fluA <- dados_cetaceos %>%
  filter(`PCR Flu (controle)` == "Positivo") %>%
  nrow()

n_positivo_h5 <- dados_cetaceos %>%
  filter(`PCR H5` == "Positivo") %>%
  nrow()

# Contagens por espécie
total_especie <- dados_cetaceos %>%
  count(Espécie, name = "Total")

positivo_fluA_especie <- dados_cetaceos %>%
  filter(`PCR Flu (controle)` == "Positivo") %>%
  count(Espécie, name = "Positivo_FluA")

positivo_h5_especie <- dados_cetaceos %>%
  filter(`PCR H5` == "Positivo") %>%
  count(Espécie, name = "Positivo_H5")

# Juntar dados
dados_plot <- total_especie %>%
  left_join(positivo_fluA_especie, by = "Espécie") %>%
  left_join(positivo_h5_especie, by = "Espécie") %>%
  mutate(
    Positivo_FluA = replace_na(Positivo_FluA, 0),
    Positivo_H5 = replace_na(Positivo_H5, 0)
  )

# Ordem das espécies
ordem_especies <- dados_plot %>%
  arrange(desc(Total)) %>%
  pull(Espécie)

dados_plot <- dados_plot %>%
  pivot_longer(
    cols = c(Total, Positivo_FluA, Positivo_H5),
    names_to = "Grupo",
    values_to = "Contagem"
  ) %>%
  mutate(
    Grupo = factor(
      Grupo,
      levels = c("Total", "Positivo_FluA", "Positivo_H5")
    ),
    Espécie = factor(
      Espécie,
      levels = ordem_especies
    )
  )

# Gráfico
ggplot(
  dados_plot,
  aes(
    x = Espécie,
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
    size = 5
  ) +
  scale_x_discrete(
    labels = function(x) parse(text = paste0("italic('", x, "')"))
  ) +
  scale_fill_manual(
    values = c(
      "Total" = "black",
      "Positivo_FluA" = "gray70",
      "Positivo_H5" = "red"
    ),
    labels = c(
      paste0("Amostrados (n = ", n_total, ")"),
      paste0("Flu-A (n = ", n_positivo_fluA, ")"),
      paste0("H5 (n = ", n_positivo_h5, ")")
    ),
    breaks = c(
      "Total",
      "Positivo_FluA",
      "Positivo_H5"
    )
  ) +
  labs(
    x = "Espécies",
    y = "Número de indivíduos",
    title = "Número de indivíduos por espécie de cetáceos positivos para Influenza A e com subtipo H5",
    fill = NULL
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.10))
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
    axis.text.x = element_text(
      angle = 50,
      hjust = 1,
      vjust = 1,
      size = 12
    ),
    axis.text.y = element_text(
      size = 16
    ),
    legend.position = "top",
    legend.text = element_text(
      size = 16,
      face = "bold"
    )
  )

ggsave(
  "Cetaceos_FluA_H5.png",
  width = 14,
  height = 8,
  dpi = 600
)

#------------------------------------------------------------------------------#
