# --------------------------------------------------- #
# |  10ª Semana de Iniciação Científica — ENCE/IBGE | #
# --------------------------------------------------- #
# --------------------------------------------------- #
# |  Análise de Sentimentos com R                   | #
# --------------------------------------------------- #
# |  Luiz Eduardo S. Gomes                          | #
# |  luiz.s.gomes@ibge.gov.br                       | #
# |  Novembro, 2026                                 | #
# --------------------------------------------------- #
# | - ANÁLISE DE PRODUTO: GTA 6 PS5 (MERCADO LIVRE) | #
# --------------------------------------------------- #

# https://www.mercadolivre.com.br/
# gta-6-grand-theft-auto-vi-playstation-5/p/MLB74108173#reviews

# Preparação do ambiente ------------------------------------------------------

## Diretorio de trabalho ----
# setwd()

## Carregamento dos pacotes ----
library(tidytext)
library(tidyverse)
library(stringi)
library(wordcloud)
library(tm)
library(udpipe)
library(syuzhet)
library(readxl)

# Corpus e documentos ---------------------------------------------------------

## Base de textos ----
avaliacoes_gta6 <- read_excel("ml_gta6/avaliacoes_gta6.xlsx") |> 
  rename(
    documento = ID,
    texto = `Comentário`
  ) |> 
  select(documento, texto) |> 
  drop_na() |> 
  mutate(
    documento = as.integer(documento),
    texto = tolower(texto),
    texto = stri_trans_general(str = texto, id = "Latin-ASCII"),
    texto = str_remove_all(string = texto, pattern = "https?\\S*|#\\S+|@\\S+"),
    texto = str_replace_all(
      string = texto, 
      pattern = "[[:digit:]]+", 
      replacement = " "
    ),
    texto = str_replace_all(
      string = texto, 
      pattern = "[[:punct:]]", 
      replacement = " "
    ),
    texto = str_replace_all(
      string = texto, 
      pattern = "[\\p{P}\\p{S}]", 
      replacement = " "
    ),
    texto = str_replace_all(
      string = texto, 
      pattern = "\\b[[:alpha:]]*([[:alpha:]])\\1{2,}[[:alpha:]]*\\b",
      replacement = " "
    ),
    texto = str_replace_all(
      string = texto, 
      pattern = "\\b[[:alpha:]]*([[:alpha:]]{2})\\1+[[:alpha:]]*\\b",
      replacement = " "
    ),
    texto = str_replace_all(
      string = texto,
      pattern = "\\\\U\\S*",
      replacement = " "
    ),
    texto = str_replace_all(
      string = texto,
      pattern = "[\\x{10000}-\\x{10FFFF}\\uFE0F]",
      replacement = " "
    ),
    texto = stringr::str_squish(texto)
  )

## Visualização inicial dos dados ----
glimpse(avaliacoes_gta6)

# Tokenização -----------------------------------------------------------------

## Tokenização por palavras usando regex -----
tokens <- avaliacoes_gta6 |>
  unnest_tokens(
    output = token,
    input = texto,
    token = "regex",
    pattern = "[^[:alpha:]-]+"
  )

# Normalização e pré-processamento --------------------------------------------

## Palavras frequentes e stopwords ----
stopwords_pt <- get_stopwords(language = "pt") |>
  rename(stopword = word) |>
  select(stopword)

## Remoção de acentos (transliteração) ----
stopwords_pt <- stopwords_pt |>
  mutate(
    stopword = stri_trans_general(
      str = stopword,
      id = "Latin-ASCII"
    )
  )

## Lematização (via modelo UDpipe) ----

# Download e carregamento do modelo UDpipe
# modelo_lemat <- udpipe_download_model(
#   language = "portuguese"
# )
# 
# modelo_pt <- udpipe_load_model(
#   file = modelo_lemat$file_model
# )

modelo_pt <- udpipe_load_model(
  file = 'portuguese-bosque-ud-2.5-191206.udpipe'
)

tokens_lematizados <- udpipe_annotate(
  object = modelo_pt,
  x = avaliacoes_gta6$texto,
  doc_id = as.character(avaliacoes_gta6$documento)
) |> 
  as_tibble() |> 
  select(doc_id, token, lemma, upos) |> 
  rename(documento = doc_id)

# rm(modelo_lemat)
rm(modelo_pt)

# Modificação necessária para a colar a lematização
tokens <- tokens |>
  group_by(documento, token) |>
  mutate(ocorrencia = row_number()) |>
  ungroup()

tokens_lematizados <- tokens_lematizados |>
  mutate(
    documento = as.integer(documento),
    token = tolower(token)
  ) |> 
  group_by(documento, token) |>
  mutate(ocorrencia = row_number()) |>
  ungroup()

# Unindo as informações
tokens <- tokens |>
  left_join(
    tokens_lematizados,
    by = c("documento", "token", "ocorrencia")
  ) |>
  select(-ocorrencia) |> 
  drop_na(lemma)

rm(tokens_lematizados)

## Remoção de stopwords ----
tokens <- tokens |>
  anti_join(
    stopwords_pt,
    by = join_by(lemma == stopword)
  ) |> 
  mutate(token = lemma) |> 
  select(- lemma)

# Análise exploratória --------------------------------------------------------

## Vocabulário ----
vocabulario <- tokens |>
  count(token, sort = TRUE)

## Visualização das palavras mais frequentes ----
vocabulario |>
  slice_max(
    order_by = n,
    n = 10,
    with_ties = FALSE
  ) |>
  ggplot(
    aes(
      x = reorder(token, n),
      y = n
    )
  ) +
  geom_col(col = "black", fill = "#583A99") +
  coord_flip() +
  labs(
    title = "Palavras mais frequentes",
    x = "Palavra",
    y = "Frequência absoluta"
  ) +
  theme_bw()

## TF-IDF ----
# Contagem por documento: n
# Frequência relativa: tf
# Importância relativa dos termos: tf-idf
tokens <- tokens |>
  group_by(documento, token) |>
  mutate(n = n()) |>
  ungroup() |> 
  bind_tf_idf(
    term = token,
    document = documento,
    n = n
  )

## Visualização do TF-IDF ----
vocabulario <- vocabulario |> 
  left_join(
    tokens |>
      group_by(token) |>
      summarise(
        tfidf_medio = mean(tf_idf),
        .groups = "drop"
      ),
    by = "token"
  )

vocabulario |>
  arrange(desc(tfidf_medio)) |> 
  slice_max(
    order_by = tfidf_medio,
    n = 10,
    with_ties = FALSE
  ) |>
  ggplot(
    aes(
      x = reorder(token, tfidf_medio),
      y = tfidf_medio
    )
  ) +
  geom_col(col = "black", fill = "#583A99") +
  coord_flip() +
  labs(
    title = "Palavras mais relevantes",
    x = "Palavra",
    y = "TF-IDF médio"
  ) +
  theme_bw()

# Análise de sentimentos com léxicos ------------------------------------------

## Construção de um léxico de sentimentos ----
lexico_nrc_pt <- get_sentiment_dictionary(
  dictionary = "nrc", 
  language = "portuguese"
) |> 
  mutate(
    word = stri_trans_general(
      str = word,
      id = "Latin-ASCII"
    )
  )

## Escore por palavra ----
tokens <- tokens |> 
  mutate(
    sentimento = get_sentiment(
      token,
      method = "nrc",
      language = "portuguese",
      lexicon = lexico_nrc_pt
    )
  )

## Intensidade e polaridade por documento ----
avaliacoes_gta6 <- avaliacoes_gta6 |> 
  left_join(
    tokens |>
      group_by(documento) |> 
      summarise(
        intensidade = sum(sentimento),
      ) |> 
      mutate(
        polaridade = case_when(
          intensidade > 0 ~ "Positivo",
          intensidade < 0 ~ "Negativo",
          TRUE ~ "Neutro",
        )
      ),
    by = "documento"
  )

# Visualização dos resultados -------------------------------------------------

## Visualização por categoria de polaridade ----
ggplot(
  avaliacoes_gta6,
  aes(
    x = factor(documento),
    y = intensidade,
    fill = forcats::fct_rev(polaridade)
  )
) + 
  geom_col(col = "black") +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  scale_fill_manual(
    values = c(
      "Positivo" = "#A1C4E0",
      "Negativo" = "#CC1A36",
      "Neutro" = "gray70"
    )
  ) +
  labs(
    title = "Sentimento das avaliações",
    x = "Documento",
    y = "Escore",
    fill = "Polaridade"
  ) +
  theme_bw()

## Distribuição das categorias de polaridade ----
avaliacoes_gta6 |>
  count(polaridade) |>
  mutate(
    percentual = 100 * n / sum(n)
  )

## Palavras que contribuem para os sentimentos ----
vocabulario <- vocabulario |> 
  left_join(
    tokens |>
      group_by(token) |> 
      summarise(
        sentimento = sum(sentimento)
      ) |> 
      ungroup(),
    by = "token"
  ) |> 
  mutate(
    polaridade = case_when(
      sentimento > 0 ~ "Positivo",
      sentimento < 0 ~ "Negativo",
      TRUE ~ "Neutro"
    )
  )

ggplot(
  vocabulario |> 
    filter(polaridade != "Neutro"),
  aes(
    x = reorder(token, sentimento),
    y = sentimento,
    fill = sentimento > 0
  )
) +
  geom_col(col = "black") +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "TRUE" = "#A1C4E0",
      "FALSE" = "#CC1A36"
    )
  ) +
  labs(
    title = "Contribuição das palavras",
    x = "Palavra",
    y = "Contribuição total"
  ) +
  theme_bw() +
  theme(
    legend.position = "none"
  )

## Nuvem de palavras baseada nas frequências ----

# Nuvem por frequencia absoluta
set.seed(2026)
wordcloud(
  words = vocabulario$token,
  freq = vocabulario$n,
  scale = c(5, 1),
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  colors = hcl.colors(
    n = 10, 
    palette = "Dark Mint", 
    rev = TRUE
  )[- (1:3)]
)

# Nuvem por TF-IDF médio
set.seed(2026)
wordcloud(
  words = vocabulario$token,
  freq = vocabulario$tfidf_medio * 100,
  scale = c(2.5, .5),
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  colors = hcl.colors(
    n = 10, 
    palette = "Dark Mint", 
    rev = TRUE
  )[- (1:3)]
)

## Nuvem de palavras baseada nas contribuições aos sentimentos ----
matriz_comparacao <- vocabulario |>
  select(token, polaridade, n) |>
  pivot_wider(
    names_from = polaridade,
    values_from = n,
    values_fill = 0,
    values_fn = sum
  ) |>
  column_to_rownames("token")

set.seed(2026)
comparison.cloud(
  matriz_comparacao,
  colors = c(
    "azure4",
    "#A1C4E0",
    "#CC1A36"
  ),
  max.words = 100,
  random.order = FALSE,
  scale = c(5, 0.8),
  title.size = 1.5,
  title.colors= c(
    "azure4",
    "#C1DBEC",
    "#CC1A36"
  )
)

# Análise de emoções com léxicos ----------------------------------------------

## Identificação das categorias emocionais ----
tokens <- tokens |> 
  mutate(
    syuzhet::get_nrc_sentiment(
      char_v = token,
      language = "portuguese"
    )
  ) |> 
  select(- negative, - positive) |> 
  rename(
    raiva = anger,
    antecipacao = anticipation,
    nojo = disgust,
    medo = fear,
    alegria = joy,
    tristeza = sadness,
    surpresa = surprise,
    confianca = trust
  )

## Distribuição agregada das emoções ----
emocoes <- tokens |>
  summarise(
    across(
      c(
        raiva, antecipacao, nojo, medo,
        alegria, tristeza, surpresa, confianca
      ),
      sum
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "emocao",
    values_to = "frequencia"
  ) |>
  arrange(desc(frequencia)) |> 
  mutate(
    percentual = 100 * frequencia / sum(frequencia)
  ) |>
  mutate(
    emocao = forcats::fct_relevel(
      emocao,
      # Positivos
      "alegria", "antecipacao", "confianca", "surpresa",
      # Negativos
      "medo", "nojo", "raiva", "tristeza"
    )
  )

## Visualização da distribuição das emoções ----
ggplot(
  emocoes,
  aes(
    x = forcats::fct_rev(emocao),
    y = frequencia,
    fill = emocao
  )
) +
  geom_col(
    colour = "black"
  ) +
  scale_fill_manual(
    values = c(
      "alegria" = "#DEEEF7",
      "antecipacao" = "#C1DBEC",
      "confianca" = "#A1C4E0",
      "surpresa" = "#7FABD3",
      "medo" = "#F3413A",
      "nojo" = "#CC1A36",
      "raiva" = "#9C0032",
      "tristeza" = "#6D0026"
    )
  ) +
  scale_x_discrete(
    labels = c(
      "raiva" = "Raiva",
      "antecipacao" = "Antecipação",
      "nojo" = "Nojo",
      "medo" = "Medo",
      "alegria" = "Alegria",
      "tristeza" = "Tristeza",
      "surpresa" = "Surpresa",
      "confianca" = "Confiança"
    )
  ) +
  coord_flip() +
  labs(
    title = "Distribuição das categorias emocionais",
    x = "Emoção",
    y = "Frequência"
  ) +
  theme_bw() +
  theme(
    legend.position = "none"
  )

## Perfis emocionais por documento ----
avaliacoes_gta6 <- avaliacoes_gta6 |> 
  left_join(
    tokens |> 
      group_by(documento) |> 
      summarise(
        across(
          c(
            raiva, antecipacao, nojo, medo,
            alegria, tristeza, surpresa, confianca
          ),
          sum
        )
      ),
    by = "documento"
  )

perfis_emocionais <- avaliacoes_gta6 |>
  select(
    documento,
    raiva, antecipacao, nojo, medo,
    alegria, tristeza, surpresa, confianca
  ) |>
  pivot_longer(
    cols = -documento,
    names_to = "emocao",
    values_to = "frequencia"
  ) |>
  group_by(documento) |>
  mutate(
    percentual = 100 * frequencia / sum(frequencia),
    percentual = replace_na(percentual, 0)
  ) |>
  ungroup() |>
  mutate(
    emocao = forcats::fct_relevel(
      emocao,
      # Positivos
      "alegria", "antecipacao", "confianca", "surpresa",
      # Negativos
      "medo", "nojo", "raiva", "tristeza"
    )
  )

## Visualização dos perfis emocionais ----
ggplot(
  perfis_emocionais,
  aes(
    x = factor(documento),
    y = forcats::fct_rev(emocao),
    fill = percentual
  )
) +
  geom_tile(color = "black") +
  scale_fill_gradient(
    low = "white",
    high = "#583A99"
  ) +
  scale_y_discrete(
    labels = c(
      "raiva" = "Raiva",
      "antecipacao" = "Antecipação",
      "nojo" = "Nojo",
      "medo" = "Medo",
      "alegria" = "Alegria",
      "tristeza" = "Tristeza",
      "surpresa" = "Surpresa",
      "confianca" = "Confiança"
    )
  ) +
  labs(
    title = "Perfis emocionais",
    x = "Documento",
    y = "Emoção",
    fill = "%"
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_colorbar(
      barwidth = 1, barheight = 8,
      frame.colour = "black",      
      frame.linewidth = .2,          
      ticks.colour = "black",
      ticks.linewidth = .2,
      title.vjust = 2.5
    )
  ) +
  theme_bw()

## Proporções emocionais ----
ggplot(
  perfis_emocionais,
  aes(
    x = factor(documento),
    y = percentual,
    fill = emocao
  )
) +
  geom_col(
    colour = "black"
  ) +
  scale_fill_manual(
    name = "Emoção",
    values = c(
      "alegria" = "#DEEEF7",
      "antecipacao" = "#C1DBEC",
      "confianca" = "#A1C4E0",
      "surpresa" = "#7FABD3",
      "medo" = "#F3413A",
      "nojo" = "#CC1A36",
      "raiva" = "#9C0032",
      "tristeza" = "#6D0026"
    ),
    labels = c(
      "raiva" = "Raiva",
      "antecipacao" = "Antecipação",
      "nojo" = "Nojo",
      "medo" = "Medo",
      "alegria" = "Alegria",
      "tristeza" = "Tristeza",
      "surpresa" = "Surpresa",
      "confianca" = "Confiança"
    )
  ) +
  labs(
    title = "Composição relativa das emoções",
    x = "Documento",
    y = "%",
    fill = "Emoção"
  ) +
  theme_bw()
