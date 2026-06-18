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
dados_flu$Geraci <- as.factor(dados_flu$Geraci)

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
      "Pinípede" = "Pinnipeds",
      "Cetáceo" = "Cetaceans",
      "Sirênio" = "Sirenians"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Total" = "black",
      "Positivo_FluA" = "gray70",
      "Positivo_H5" = "red"
    ),
    labels = c(
      paste0("Sampled (n = ", n_total, ")"),
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
    x = "Taxons",
    y = "Number of individuals",
    title = "Number of individuals positive for Influenza A and the H5 subtype",
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
      paste0("Sampled (n = ", n_total, ")"),
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
    x = "species",
    y = "Number of individuals",
    title = "Number of individuals per species positive for Influenza A and the H5 subtype",
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
      paste0("Sampled (n = ", n_total, ")"),
      paste0("Flu-A (n = ", n_positivo_fluA, ")"),
      paste0("H5 (n = ", n_positivo_h5, ")")
    )
  ) +
  labs(
    x = "Species",
    y = "Number of individuals",
    title = "Number of pinnipeds individuals per species positive for Influenza A and the H5 subtype",
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
      paste0("Sampled (n = ", n_total, ")"),
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
    x = "Species",
    y = "Number of individuals",
    title = "Number of cetacean individuals per species positive for Influenza A and the H5 subtype",
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

# Colunas dos swabs
swabs <- c(
  "Cerebral",
  "Cerebelo",
  "Ocular",
  "Nasal",
  "Oral",
  "Traqueal",
  "Pulmonar",
  "Brônquios",
  "Glândula Mamária",
  "Genital",
  "Anal",
  "Intestino Delgado",
  "Intestino Grosso",
  "Fezes",
  "Edema",
  "Ferida"
)

# Swabs coletados (1 e 2)
total_swabs <- dados_flu %>%
  select(all_of(swabs)) %>%
  pivot_longer(
    everything(),
    names_to = "Região",
    values_to = "Resultado"
  ) %>%
  filter(Resultado %in% c("1", "2")) %>%
  count(Região, name = "Total")

# Swabs Flu-A positivos
fluA_swabs <- dados_flu %>%
  filter(`PCR Flu (controle)` == "Positivo") %>%
  select(all_of(swabs)) %>%
  pivot_longer(
    everything(),
    names_to = "Região",
    values_to = "Resultado"
  ) %>%
  filter(Resultado == "2") %>%
  count(Região, name = "FluA")

# Swabs H5 positivos
h5_swabs <- dados_flu %>%
  filter(`PCR H5` == "Positivo") %>%
  select(all_of(swabs)) %>%
  pivot_longer(
    everything(),
    names_to = "Região",
    values_to = "Resultado"
  ) %>%
  filter(Resultado == "2") %>%
  count(Região, name = "H5")

# Totais para legenda
n_total_swabs <- sum(total_swabs$Total)

n_fluA_swabs <- fluA_swabs %>%
  summarise(total = sum(FluA)) %>%
  pull(total)

n_h5_swabs <- h5_swabs %>%
  summarise(total = sum(H5)) %>%
  pull(total)

# Juntar dados
dados_plot <- total_swabs %>%
  left_join(fluA_swabs, by = "Região") %>%
  left_join(h5_swabs, by = "Região") %>%
  mutate(
    FluA = ifelse(is.na(FluA), 0, FluA),
    H5 = ifelse(is.na(H5), 0, H5)
  )

# Ordem baseada no total coletado
ordem_regioes <- dados_plot %>%
  arrange(desc(Total)) %>%
  pull(Região)

# Converter para formato longo
dados_plot <- dados_plot %>%
  pivot_longer(
    cols = c(Total, FluA, H5),
    names_to = "Grupo",
    values_to = "Contagem"
  ) %>%
  mutate(
    Grupo = factor(
      Grupo,
      levels = c("Total", "FluA", "H5")
    ),
    Região = factor(
      Região,
      levels = ordem_regioes
    )
  )

# Gráfico
# Gráfico
ggplot(
  dados_plot,
  aes(
    x = Região,
    y = Contagem,
    fill = Grupo
  )
) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.85)
  ) +
  geom_text(
    aes(label = Contagem),
    position = position_dodge(width = 0.85),
    vjust = -0.3,
    size = 5
  ) +
  scale_x_discrete(
    labels = c(
      "Cerebral" = "Cerebrum",
      "Cerebelo" = "Cerebellum",
      "Ocular" = "Ocular",
      "Nasal" = "Nasal",
      "Oral" = "Oral",
      "Traqueal" = "Tracheal",
      "Pulmonar" = "Lung",
      "Brônquios" = "Bronchi",
      "Glândula Mamária" = "Mammary gland",
      "Genital" = "Genital",
      "Anal" = "Anal",
      "Intestino Delgado" = "Small intestine",
      "Intestino Grosso" = "Large intestine",
      "Fezes" = "Feces",
      "Edema" = "Edema",
      "Ferida" = "Wound"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Total" = "black",
      "FluA" = "gray70",
      "H5" = "red"
    ),
    labels = c(
      paste0("Collected swabs (n = ", n_total_swabs, ")"),
      paste0("Flu-A (n = ", n_fluA_swabs, ")"),
      paste0("H5 (n = ", n_h5_swabs, ")")
    ),
    breaks = c(
      "Total",
      "FluA",
      "H5"
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    x = "Sampled regions",
    y = "Number of samples",
    title = "Number of collected swabs and swabs positive for Influenza A and the H5 subtype",
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
      angle = 45,
      hjust = 1,
      size = 14
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

# Salvar figura
ggsave(
  "Swabs_H5_English.png",
  width = 14,
  height = 8,
  dpi = 600
)

#------------------------------------------------------------------------------#

dados_resumo <- dados_flu %>%
  select(
    `Data da coleta`,
    Espécie,
    Táxon,
    `PCR Flu (controle)`,
    `PCR H5`
  ) %>%
  filter(
    !is.na(`Data da coleta`),
    `Data da coleta` != "-"
  ) %>%
  mutate(
    `Data da coleta` = as.Date(
      as.numeric(`Data da coleta`),
      origin = "1899-12-30"
    )
  )

str(dados_resumo)

#------------------------------------------------------------------------------#

dados_plot <- dados_resumo %>%
  filter(
    `PCR Flu (controle)` == "Positivo"
  ) %>%
  mutate(
    Resultado = case_when(
      `PCR H5` == "Positivo" ~ "H5",
      `PCR Flu (controle)` == "Positivo" ~ "Flu-A"
    )
  )

#------------------------------------------------------------------------------#

# Totais para a legenda
n_fluA <- dados_resumo %>%
  filter(`PCR Flu (controle)` == "Positivo") %>%
  nrow()

n_h5 <- dados_resumo %>%
  filter(`PCR H5` == "Positivo") %>%
  nrow()

#------------------------------------------------------------------------------#

# Totais para a legenda
n_fluA <- dados_plot %>%
  filter(Resultado == "Flu-A") %>%
  nrow()

n_h5 <- dados_plot %>%
  filter(Resultado == "H5") %>%
  nrow()

# Agrupar por mês
dados_plot_mes <- dados_plot %>%
  mutate(
    Mes = as.Date(format(`Data da coleta`, "%Y-%m-01"))
  ) %>%
  count(
    Mes,
    Espécie,
    Resultado,
    name = "Contagem"
  )

# Gráfico
ggplot() +
  
  # Flu-A primeiro (fica atrás)
  geom_point(
    data = dados_plot_mes %>%
      filter(Resultado == "Flu-A"),
    aes(
      x = Mes,
      y = Espécie,
      size = Contagem,
      fill = Resultado
    ),
    shape = 21,
    color = "black",
    stroke = 1.5,
    alpha = 0.9
  ) +
  
  # H5 depois (fica na frente)
  geom_point(
    data = dados_plot_mes %>%
      filter(Resultado == "H5"),
    aes(
      x = Mes,
      y = Espécie,
      size = Contagem,
      fill = Resultado
    ),
    shape = 21,
    color = "black",
    stroke = 1.5,
    alpha = 0.9
  ) +
  
  # Cores das classes
  scale_fill_manual(
    values = c(
      "Flu-A" = "gray60",
      "H5" = "red"
    ),
    labels = c(
      "Flu-A",
      "H5"
    ),
    breaks = c(
      "Flu-A",
      "H5"
    ),
    name = NULL
  ) +
  
  # Escala de tamanho dos pontos
  scale_size_continuous(
    name = "Número de positivos",
    range = c(5, 15),
    breaks = pretty(dados_plot_mes$Contagem),
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      ncol = 1
    )
  ) +
  
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%Y-%m"
  ) +
  
  labs(
    x = "Data da coleta",
    y = "Espécie",
    fill = NULL,
    title = "Mamíferos marinhos positivos para Influenza A e H5"
  ) +
  
  guides(
    fill = guide_legend(
      nrow = 1,
      byrow = TRUE,
      title = NULL,
      order = 1,
      override.aes = list(
        size = 8,      # tamanho dos círculos da legenda
        shape = 21,
        color = "black",
        stroke = 1.5
      )
    ),
    size = guide_legend(
      order = 2
    )
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
      angle = 45,
      hjust = 1,
      size = 12
    ),
    
    axis.text.y = element_text(
      face = "italic",
      size = 14
    ),
    
    legend.position = "right",
    legend.box = "vertical",
    legend.box.just = "center",
    
    legend.title = element_text(
      size = 14,
      face = "bold"
    ),
    
    legend.text = element_text(
      size = 13
    )
  )

#------------------------------------------------------------------------------#

# Salvar figura (opcional)
ggsave(
  "FluA_H5_timeline.png",
  width = 14,
  height = 8,
  dpi = 600
)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Dados agregados
#------------------------------------------------------------------------------#

dados_plot_mes <- dados_resumo %>%
  mutate(
    Resultado = case_when(
      `PCR Flu (controle)` == "Positivo" & `PCR H5` == "Positivo" ~ "H5",
      `PCR Flu (controle)` == "Positivo" ~ "Flu-A",
      TRUE ~ "Negativo"
    ),
    Mes = as.Date(format(`Data da coleta`, "%Y-%m-01"))
  ) %>%
  count(Mes, Espécie, Resultado, name = "Contagem") %>%
  mutate(
    Resultado = factor(Resultado, levels = c("Negativo", "Flu-A", "H5"))
  )

#------------------------------------------------------------------------------#
# Gráfico (UMA camada → ordem garantida)
#------------------------------------------------------------------------------#

ggplot() +
  
  # --------------------------#
  # Negativos (fundo)
  # --------------------------#
  geom_point(
    data = dados_plot_mes %>% filter(Resultado == "Negativo"),
    aes(
      x = Mes,
      y = Espécie,
      size = Contagem,
      fill = Resultado
    ),
    shape = 21,
    color = "black",
    stroke = 1.4
  ) +
  
  # --------------------------#
  # Flu-A (meio)
  # --------------------------#
  geom_point(
    data = dados_plot_mes %>% filter(Resultado == "Flu-A"),
    aes(
      x = Mes,
      y = Espécie,
      size = Contagem,
      fill = Resultado
    ),
    shape = 21,
    color = "black",
    stroke = 1.4
  ) +
  
  # --------------------------#
  # H5 (topo)
  # --------------------------#
  geom_point(
    data = dados_plot_mes %>% filter(Resultado == "H5"),
    aes(
      x = Mes,
      y = Espécie,
      size = Contagem,
      fill = Resultado
    ),
    shape = 21,
    color = "black",
    stroke = 1.4
  ) +
  
  # Cores fixas
  scale_fill_manual(
    values = c(
      "Negativo" = "white",
      "Flu-A" = "gray60",
      "H5" = "red"
    ),
    name = NULL
  ) +
  
  # Tamanho dos pontos
  scale_size_continuous(
    name = "Number of samples",
    range = c(4, 14)
  ) +
  
  # Eixos
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%Y-%m"
  ) +
  
  labs(
    x = "Sampling period",
    y = "Species"
  ) +
  
  guides(
    fill = guide_legend(
      nrow = 1,
      override.aes = list(
        shape = 21,
        color = "black",
        size = 7,
        stroke = 1.2
      )
    ),
    
    size = guide_legend(
      order = 2
    )
  ) +
  
  theme_classic(base_size = 18) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(face = "italic"),
    
    # 🔥 LEGENDA FORA DO GRÁFICO, TOPO CENTRALIZADA
    legend.position = "top",
    legend.justification = "center",
    legend.box = "horizontal",
    
    legend.title = element_blank()
  )

#------------------------------------------------------------------------------#

# Salvar figura (opcional)
ggsave(
  "FluA_H5_timeline.png",
  width = 14,
  height = 8,
  dpi = 600
)

#------------------------------------------------------------------------------#

# Total de indivíduos amostrados
n_total <- nrow(dados_flu)

# Total de positivos Flu-A
n_positivo_fluA <- dados_flu %>%
  filter(`PCR Flu (controle)` == "Positivo") %>%
  nrow()

# Total de positivos H5
n_positivo_h5 <- dados_flu %>%
  filter(`PCR H5` == "Positivo") %>%
  nrow()

# Contagem total por categoria Geraci
total_geraci <- dados_flu %>%
  count(Geraci, name = "Total")

# Contagem de positivos Flu-A por categoria Geraci
positivo_fluA_geraci <- dados_flu %>%
  filter(`PCR Flu (controle)` == "Positivo") %>%
  count(Geraci, name = "Positivo_FluA")

# Contagem de positivos H5 por categoria Geraci
positivo_h5_geraci <- dados_flu %>%
  filter(`PCR H5` == "Positivo") %>%
  count(Geraci, name = "Positivo_H5")

# Juntar contagens
dados_plot_geraci <- total_geraci %>%
  left_join(positivo_fluA_geraci, by = "Geraci") %>%
  left_join(positivo_h5_geraci, by = "Geraci") %>%
  mutate(
    Positivo_FluA = ifelse(is.na(Positivo_FluA), 0, Positivo_FluA),
    Positivo_H5 = ifelse(is.na(Positivo_H5), 0, Positivo_H5)
  )

# Ordenar categorias do maior para o menor,
# mantendo "Sem" sempre na última posição
ordem_geraci <- dados_plot_geraci %>%
  arrange(desc(Total)) %>%
  pull(Geraci) %>%
  unique()

ordem_geraci <- c(
  ordem_geraci[ordem_geraci != "Sem"],
  "Sem"
)

# Converter para formato longo
dados_plot_geraci <- dados_plot_geraci %>%
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
    Geraci = factor(
      Geraci,
      levels = ordem_geraci
    )
  )

# Gráfico
ggplot(
  dados_plot_geraci,
  aes(
    x = Geraci,
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
  scale_fill_manual(
    values = c(
      "Total" = "black",
      "Positivo_FluA" = "gray70",
      "Positivo_H5" = "red"
    ),
    labels = c(
      paste0("Sampled (n = ", n_total, ")"),
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
    x = "Geraci Code",
    y = "Number of individuals",
    title = "Number of individuals positive for Influenza A and the H5 subtype by Geraci code",
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
      #angle = 45,
      hjust = 1,
      size = 16
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

#------------------------------------------------------------------------------#

# Salvar figura (opcional)
ggsave(
  "FluA_H5_GERACI.png",
  width = 14,
  height = 8,
  dpi = 600
)

#------------------------------------------------------------------------------#
