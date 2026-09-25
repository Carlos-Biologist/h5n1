# 1) Pacotes
# install.packages(c("readxl", "dplyr"))  # rode uma vez, se ainda não tiver instalado
library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)
library(tidyr)

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

# 4) Padronizar a coluna: remove espaços e trata "", "-" e "N/A" como ausência de laudo
dados_laudos <- dados %>%
  mutate(link_laudo = trimws(as.character(`Hiperlink para acessar laudo`)),
         link_laudo = if_else(link_laudo %in% c("", "-") | toupper(link_laudo) == "N/A",
                              NA_character_, link_laudo))

str(dados_laudos)

# Salvar o objeto como arquivo Excel
write_xlsx(dados_laudos, "dados_laudos.xlsx")

# Número de laudos únicos (hiperlinks repetidos contam como 1)
n_laudos <- n_distinct(dados_laudos$link_laudo, na.rm = TRUE)
n_laudos

# Número de amostras (IDs únicos) que têm laudo
ids_com_laudo <- unique(dados_laudos$ID[!is.na(dados_laudos$link_laudo)])
length(ids_com_laudo)

# Número de amostras sem nenhum laudo
ids_sem_laudo <- setdiff(unique(dados_laudos$ID), ids_com_laudo)
length(ids_sem_laudo)

# ---------------------------------------------------------------------------- #

# Contagem de amostras (IDs únicos) com laudo, por órgão
contagem_orgao <- dados_laudos %>%
  filter(!is.na(link_laudo), !is.na(Orgão)) %>%
  group_by(Orgão) %>%
  summarise(n = n_distinct(ID), .groups = "drop") %>%
  arrange(desc(n))

# Gráfico de colunas em ordem decrescente, com a contagem no topo
fonte <- "Times New Roman"

orgao_plot <- ggplot(contagem_orgao, aes(x = reorder(Orgão, -n), y = n)) +
  geom_col(fill = "black") +
  geom_text(aes(label = n), vjust = -0.5,
            family = fonte, size = 12 / .pt) +   # 12 pt nos números do topo
  scale_y_continuous(breaks = scales::breaks_width(20),   # escala de 20 em 20
                     expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Órgão", y = "Número amostrado de tecidos") +
  theme_classic(base_family = fonte, base_size = 12) +
  theme(
    text        = element_text(family = fonte, size = 12),
    axis.title  = element_text(size = 12),
    axis.text   = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
  )

orgao_plot

ggsave(
  filename = "grafico_orgaos.png",
  plot     = orgao_plot,
  width    = 12,      # largura em polegadas
  height   = 7,       # altura em polegadas
  dpi      = 600,     # alta resolução (300 já é o padrão para publicação)
  units    = "in",
  bg       = "white"  # fundo branco, evita fundo transparente
)

# ---------------------------------------------------------------------------- #

# Contagem de amostras (IDs únicos) com laudo, por classe de diagnóstico
contagem_classe <- dados_laudos %>%
  mutate(Laudo_class = trimws(as.character(Laudo_class))) %>%
  filter(!is.na(link_laudo),
         !is.na(Laudo_class),
         Laudo_class != "-") %>%                 # remove "-" do gráfico
  distinct(ID, Laudo_class) %>%
  count(Laudo_class, name = "n") %>%
  arrange(desc(n))

str(contagem_classe)

fonte <- "Times New Roman"

classe_plot <- ggplot(contagem_classe, aes(x = reorder(Laudo_class, -n), y = n)) +
  geom_col(fill = "black") +
  geom_text(aes(label = n), vjust = -0.5,
            family = fonte, size = 12 / .pt) +
  scale_y_continuous(breaks = scales::breaks_width(20),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Laudo descritivo resumido", y = "Número de amostras") +
  theme_classic(base_family = fonte, base_size = 12) +
  theme(
    text        = element_text(family = fonte, size = 12),
    axis.title  = element_text(size = 12),
    axis.text   = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
  )

classe_plot

ggsave("grafico_laudo_resumidos_2.png", classe_plot,
       width = 12, height = 7, dpi = 600, units = "in", bg = "white")

# ---------------------------------------------------------------------------- #

# Contagem de amostras (IDs únicos) com laudo, por classe de diagnóstico
contagem_agrup <- dados_laudos %>%
  mutate(Laudo_descritivo_agrup = trimws(as.character(`Laudo descritivo_agrup`))) %>%
  filter(!is.na(link_laudo),
         !is.na(`Laudo descritivo_agrup`),
         `Laudo descritivo_agrup` != "-") %>%                 # remove "-" do gráfico
  distinct(ID, `Laudo descritivo_agrup`) %>%
  count(`Laudo descritivo_agrup`, name = "n") %>%
  arrange(desc(n))

str(contagem_agrup)

fonte <- "Times New Roman"

laudo_agrup_plot <- ggplot(contagem_agrup, aes(x = reorder(`Laudo descritivo_agrup`, -n), y = n)) +
  geom_col(fill = "black") +
  geom_text(aes(label = n), vjust = -0.5,
            family = fonte, size = 12 / .pt) +
  scale_y_continuous(breaks = scales::breaks_width(20),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Laudo descritivo resumido", y = "Número de amostras") +
  theme_classic(base_family = fonte, base_size = 12) +
  theme(
    text        = element_text(family = fonte, size = 12),
    axis.title  = element_text(size = 12),
    axis.text   = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
  )

laudo_agrup_plot

ggsave("grafico_laudo_resumido_1.png", laudo_agrup_plot,
       width = 12, height = 7, dpi = 600, units = "in", bg = "white")

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# 5) Carregar a planilha
dados_laudos_analise <- read_excel("dados_laudos.xlsx")

# Conferir a estrutura
str(dados_laudos_analise)

dados_laudos_analise$Gripe_class <- as.factor(dados_laudos_analise$Gripe_class)

contagem_pcr <- dados_laudos_analise%>%
  distinct(ID, `PCR Flu (controle)`) %>%     # valor repetido no mesmo ID conta uma vez
  count(`PCR Flu (controle)`, name = "n_amostras")

contagem_pcr

# ---------------------------------------------------------------------------- #

# Contagem de amostras (IDs únicos) com laudo, por órgão
contagem_orgao_analise <- dados_laudos_analise %>%
  filter(!is.na(link_laudo), !is.na(Orgão)) %>%
  group_by(Orgão) %>%
  summarise(n = n_distinct(ID), .groups = "drop") %>%
  arrange(desc(n))

# Gráfico de colunas em ordem decrescente, com a contagem no topo
fonte <- "Times New Roman"

orgao_analise_plot <- ggplot(contagem_orgao_analise, aes(x = reorder(Orgão, -n), y = n)) +
  geom_col(fill = "black") +
  geom_text(aes(label = n), vjust = -0.5,
            family = fonte, size = 12 / .pt) +   # 12 pt nos números do topo
  scale_y_continuous(breaks = scales::breaks_width(20),   # escala de 20 em 20
                     expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Órgão", y = "Número amostrado de tecidos") +
  theme_classic(base_family = fonte, base_size = 12) +
  theme(
    text        = element_text(family = fonte, size = 12),
    axis.title  = element_text(size = 12),
    axis.text   = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
  )

orgao_analise_plot

ggsave(
  filename = "grafico_orgaos_analise.png",
  plot     = orgao_plot,
  width    = 12,      # largura em polegadas
  height   = 7,       # altura em polegadas
  dpi      = 600,     # alta resolução (300 já é o padrão para publicação)
  units    = "in",
  bg       = "white"  # fundo branco, evita fundo transparente
)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

library(dplyr)
library(vcd)     # para mosaico
# install.packages("vcd")  # rode uma vez, se necessário

# Distribuição da variável resposta
table(dados_laudos_analise$Gripe_class)

# Tabelas de contingência (contagem por amostra, ID único)
tab_orgao <- dados_laudos_analise %>%
  distinct(ID, Orgão, Gripe_class) %>%
  count(Orgão, Gripe_class) %>%
  tidyr::pivot_wider(names_from = Gripe_class, values_from = n, values_fill = 0)
tab_orgao

tab_laudo_class <- dados_laudos_analise %>%
  distinct(ID, Laudo_class, Gripe_class) %>%
  count(Laudo_class, Gripe_class) %>%
  tidyr::pivot_wider(names_from = Gripe_class, values_from = n, values_fill = 0)
tab_laudo_class

tab_lesao_agrup <- dados_laudos_analise %>%
  distinct(ID, Lesão_agrup, Gripe_class) %>%
  count(Lesão_agrup, Gripe_class) %>%
  tidyr::pivot_wider(names_from = Gripe_class, values_from = n, values_fill = 0)
tab_lesao_agrup

# ---------------------------------------------------------------------------- #

# Mosaico (visual) — exemplo com Orgão
tab_mosaico <- table(
  dados_laudos_analise %>% distinct(ID, Orgão, Gripe_class) %>% pull(Orgão),
  dados_laudos_analise %>% distinct(ID, Orgão, Gripe_class) %>% pull(Gripe_class)
)

# ---------------------------------------------------------------------------- #

mosaicplot(tab_mosaico, shade = TRUE, las = 2,
           main = "Órgão x Gripe_class",
           xlab = "", ylab = "Gripe_class")

df_mosaico <- as.data.frame(tab_mosaico)
names(df_mosaico) <- c("Orgao", "Gripe_class", "Freq")

mosaic(Freq ~ Orgao + Gripe_class, data = df_mosaico, shade = TRUE, legend = TRUE)

# ---------------------------------------------------------------------------- #

dados_pos_neg <- dados_laudos_analise %>%
  mutate(Grupo = if_else(Gripe_class == "0", "Negativo", "Positivo"))

# Salvar o objeto como arquivo Excel
write_xlsx(dados_pos_neg, "dados_pos_neg.xlsx")

# Conferir os grupos (amostras únicas)
dados_pos_neg %>% distinct(ID, Grupo) %>% count(Grupo)

# Fisher exato por Órgão
tab_orgao_grupo <- dados_pos_neg %>%
  distinct(ID, Orgão, Grupo) %>%
  count(Orgão, Grupo) %>%
  tidyr::pivot_wider(names_from = Grupo, values_from = n, values_fill = 0)

# ---------------------------------------------------------------------------- #

# Exemplo: testar um órgão específico (ex.: Pulmão) presença/ausência x Grupo
testar_orgao <- function(orgao_nome) {
  tab <- dados_pos_neg %>%
    distinct(ID, Grupo) %>%
    left_join(
      dados_pos_neg %>% filter(Orgão == orgao_nome) %>% distinct(ID) %>% mutate(presente = "Sim"),
      by = "ID"
    ) %>%
    mutate(presente = if_else(is.na(presente), "Não", presente)) %>%
    count(presente, Grupo) %>%
    tidyr::pivot_wider(names_from = Grupo, values_from = n, values_fill = 0)
  
  tab_matriz <- as.matrix(tab[,-1])
  rownames(tab_matriz) <- tab$presente
  
  list(tabela = tab, teste = fisher.test(tab_matriz))
}

resultado_pulmao <- testar_orgao("Pulmão")
resultado_pulmao$tabela
resultado_pulmao$teste

# ---------------------------------------------------------------------------- #

orgaos_interesse <- c("Pulmão", "Traqueia", "Fígado", "Baço", "Rim",
                      "Coração", "Cérebro", "Músculo esquelético", "Linfonodo")

resultados <- lapply(orgaos_interesse, function(o) {
  r <- testar_orgao(o)
  data.frame(
    Orgão      = o,
    odds_ratio = unname(r$teste$estimate),
    IC_inf     = r$teste$conf.int[1],
    IC_sup     = r$teste$conf.int[2],
    p_valor    = r$teste$p.value
  )
})

resultados_df <- bind_rows(resultados) %>%
  arrange(p_valor) %>%
  mutate(p_ajustado = p.adjust(p_valor, method = "BH"))

resultados_df

# ---------------------------------------------------------------------------- #

modelo_orgao <- glm(Gripe_class ~ Orgão)
# Contagem de amostras únicas (ID) por Órgão e Grupo
contagem_orgao_grupo <- dados_pos_neg %>%
  distinct(ID, Orgão, Grupo) %>%
  count(Orgão, Grupo, name = "n")

# Ordenar órgãos pelo total (Negativo + Positivo), decrescente
ordem_orgaos <- contagem_orgao_grupo %>%
  group_by(Orgão) %>%
  summarise(total = sum(n)) %>%
  arrange(desc(total)) %>%
  pull(Orgão)

contagem_orgao_grupo <- contagem_orgao_grupo %>%
  mutate(Orgão = factor(Orgão, levels = ordem_orgaos))

fonte <- "Times New Roman"

grafico_orgao_grupo <- ggplot(contagem_orgao_grupo, aes(x = Orgão, y = n, fill = Grupo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = n), position = position_dodge(width = 0.8),
            vjust = -0.5, family = fonte, size = 10 / .pt) +
  scale_fill_manual(values = c("Negativo" = "black", "Positivo" = "grey60")) +
  scale_y_continuous(breaks = scales::breaks_width(10),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Órgão", y = "Número de amostras") +
  theme_classic(base_family = fonte, base_size = 12) +
  theme(
    text        = element_text(family = fonte, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.title = element_blank()
  )

grafico_orgao_grupo

ggsave("grafico_orgao_negativo_positivo.png", grafico_orgao_grupo,
       width = 16, height = 8, dpi = 600, units = "in", bg = "white")

# ---------------------------------------------------------------------------- #

# 1) Base: uma linha por amostra, com Grupo
base_id <- dados_pos_neg %>%
  distinct(ID, Grupo)

# 2) Para cada órgão de interesse, presença/ausência por amostra
presenca_orgaos <- dados_pos_neg %>%
  filter(Orgão %in% orgaos_interesse) %>%
  distinct(ID, Orgão) %>%
  mutate(presente = 1) %>%
  pivot_wider(names_from = Orgão, values_from = presente, values_fill = 0)

# 3) Juntar tudo: uma linha por amostra, colunas = Grupo + presença por órgão
dados_glm <- base_id %>%
  left_join(presenca_orgaos, by = "ID") %>%
  mutate(across(all_of(orgaos_interesse), ~ replace_na(.x, 0)))

# Conferir
glimpse(dados_glm)

dados_glm <- dados_glm %>%
  mutate(Grupo = factor(Grupo, levels = c("Negativo", "Positivo")))

# Salvar o objeto como arquivo Excel
write_xlsx(dados_glm, "dados_glm_orgão.xlsx")

modelo_glm <- glm(
  Grupo ~ Pulmão + Traqueia + Fígado + Baço + Rim +
    Coração + Cérebro + `Músculo esquelético` + Linfonodo,
  data = dados_glm,
  family = binomial
)

summary(modelo_glm)

# 5) Odds ratios e intervalos de confiança (mais fácil de interpretar que os coeficientes brutos)
exp(cbind(odds_ratio = coef(modelo_glm), confint(modelo_glm)))

# ---------------------------------------------------------------------------- #

# 1) Ranking bivariado (retomando os resultados do Fisher exato já calculados)
resultados_df %>% arrange(p_valor)

# 2) Seleção stepwise por AIC, a partir do modelo completo já ajustado
modelo_step <- step(modelo_glm, direction = "backward", trace = TRUE)

# Ver o modelo final escolhido
summary(modelo_step)
formula(modelo_step)

# 3) Comparar o modelo completo com o reduzido (teste de razão de verossimilhança)
anova(modelo_step, modelo_glm, test = "Chisq")

exp(cbind(odds_ratio = coef(modelo_step), confint(modelo_step)))

# ---------------------------------------------------------------------------- #

contagem_cerebro <- dados_laudos_analise%>%
  distinct(ID, `Orgão`) %>%     # valor repetido no mesmo ID conta uma vez
  count(`Orgão`, name = "n_amostras")

contagem_cerebro

# ---------------------------------------------------------------------------- #

dados_cerebro <- dados_laudos_analise %>%
  filter(Orgão == "Cérebro")

contagem_cerebro_class <- dados_cerebro%>%
  distinct(ID, `Orgão`) %>%     # valor repetido no mesmo ID conta uma vez
  count(`Orgão`, name = "n_amostras")

contagem_cerebro_class

dados_cerebro %>%
  count(Laudo_class, sort = TRUE)

# ---------------------------------------------------------------------------- #

frequencia_laudo_class <- dados_laudos_analise %>%
  filter(Orgão == "Cérebro") %>%
  distinct(ID, Laudo_class) %>%
  count(Laudo_class, sort = TRUE)

# ---------------------------------------------------------------------------- #

