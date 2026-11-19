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
# | - TUTORIAL                                      | #
# --------------------------------------------------- #

# Preparação do ambiente ------------------------------------------------------

## Diretorio de trabalho ----
# setwd()

## Instalação dos pacotes ----
# install.packages("tidytext")
# install.packages("tidyverse")
# install.packages("stringi")
# install.packages("wordcloud")
# install.packages("syuzhet")
# install.packages("tm")
# install.packages("udpipe")

## Carregamento dos pacotes ----
library(tidytext)
library(tidyverse)
library(stringi)
library(wordcloud)
library(tm)
library(udpipe)
library(syuzhet)

# Corpus e documentos ---------------------------------------------------------

## Construção de uma base de textos ----
avaliacoes <- tibble(
  documento = 1:10,
  texto = c(
    "O atendimento foi excelente e rápido!",
    "O serviço foi muito bom.",
    "A equipe foi atenciosa e eficiente.",
    "O atendimento foi péssimo e demorado.",
    "Os serviços foram ruins e confusos.",
    "A experiência foi frustrante!",
    "O atendimento começou às 8 horas.",
    "O serviço foi realizado na segunda-feira.",
    "O atendimento não foi bom!",
    "O serviço não foi ruim, mas foi lento."
  )
)

## Visualização inicial dos dados ----
glimpse(avaliacoes)

# Tokenização -----------------------------------------------------------------

## Tokenização por palavras -----
tokens <- avaliacoes |>
  unnest_tokens(
    output = token,
    input = texto,
    token = "words"
  )

## A estrutura tidy text
tokens |>
  filter(documento == 1)

tokens |>
  filter(token == "atendimento")

tokens |>
  count(documento)

## Tokenização por n-gramas ----
tokens_bigramas <- avaliacoes |>
  unnest_tokens(
    output = bigrama,
    input = texto,
    token = "ngrams",
    n = 2
  )

## Tokenização por sentenças ----
tokens_sentencas <- avaliacoes |>
  unnest_tokens(
    output = sentenca,
    input = texto,
    token = "sentences"
  )

## Tokenização por palavras usando regex -----
tokens <- avaliacoes |>
  unnest_tokens(
    output = token,
    input = texto,
    token = "regex",
    pattern = "[^[:alpha:]-]+"
  )

rm(tokens_bigramas, tokens_sentencas)

# Normalização e pré-processamento --------------------------------------------

## Palavras frequentes e stopwords ----
stopwords_pt <- get_stopwords(language = "pt") |>
  rename(stopword = word) |>
  select(stopword)

## Remoção de números ----
tokens <- tokens |> 
  mutate(
    token = removeNumbers(token)
  ) |> 
  filter(token != "")

## Lematização (via modelo UDpipe) ----

# Download e carregamento do modelo UDpipe
modelo_lemat <- udpipe_download_model(
  language = "portuguese"
)

modelo_pt <- udpipe_load_model(
  file = modelo_lemat$file_model
)

tokens_lematizados <- udpipe_annotate(
  object = modelo_pt,
  x = avaliacoes$texto,
  doc_id = as.character(avaliacoes$documento)
) |> 
  as_tibble() |> 
  select(doc_id, token, lemma, upos) |> 
  rename(documento = doc_id)

rm(modelo_lemat)
rm(modelo_pt)

# Verificando diferenças entre os arquivos
print(tokens)
print(tokens_lematizados)

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
    by = join_by(token == stopword)
  )

# Análise exploratória --------------------------------------------------------

## Matriz documento-termo ----
dtm <- tokens |>
  count(documento, token) |>
  cast_dtm(
    document = documento,
    term = token,
    value = n
  )

# Visualizar DTM
inspect(dtm)
# as.matrix(dtm)

rm(dtm)

## Tokens por documento ----
tokens |>
  group_by(documento) |>
  summarise(
    n_distintas = n_distinct(token),
    .groups = "drop"
  )

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
)

## Escore por palavra ----
tokens <- tokens |> 
  mutate(
    sentimento_token = get_sentiment(
      token,
      method = "nrc",
      language = "portuguese",
      lexicon = lexico_nrc_pt
    ),
    sentimento_lemma = get_sentiment(
      lemma,
      method = "nrc",
      language = "portuguese",
      lexicon = lexico_nrc_pt
    )
  )

## Intensidade e polaridade por documento ----
avaliacoes <- avaliacoes |> 
  left_join(
    tokens |>
      group_by(documento) |> 
      summarise(
        intensidade_token = sum(sentimento_token),
        intensidade_lemma = sum(sentimento_lemma),
      ) |> 
      mutate(
        polaridade_token = case_when(
          intensidade_token > 0 ~ "Positivo",
          intensidade_token < 0 ~ "Negativo",
          TRUE ~ "Neutro",
        ),
        polaridade_lemma = case_when(
          intensidade_lemma > 0 ~ "Positivo",
          intensidade_lemma < 0 ~ "Negativo",
          TRUE ~ "Neutro",
        )
      ),
    by = "documento"
  )
  
# Visualização dos resultados -------------------------------------------------

## Visualização por categoria de polaridade ----
ggplot(
  avaliacoes,
  aes(
    x = factor(documento),
    y = intensidade_lemma,
    fill = forcats::fct_rev(polaridade_lemma)
  )
) + 
  geom_col(col = "black") +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  scale_fill_manual(
    values = c(
      "Positivo" = "#C1DBEC",
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
avaliacoes |>
  count(polaridade_lemma) |>
  mutate(
    percentual = 100 * n / sum(n)
  )

## Palavras que contribuem para os sentimentos ----
vocabulario_lemma <- tokens |>
  count(lemma, sort = TRUE) |>
  left_join(
    tokens |>
      group_by(lemma) |>
      summarise(
        tfidf_medio = mean(tf_idf),
        .groups = "drop"
      ),
    by = "lemma"
  ) |> 
  left_join(
    tokens |>
      group_by(lemma) |> 
      summarise(
        sentimento_lemma = sum(sentimento_lemma)
      ) |> 
      ungroup(),
    by = "lemma"
  ) |> 
  mutate(
    polaridade_lemma = case_when(
      sentimento_lemma > 0 ~ "Positivo",
      sentimento_lemma < 0 ~ "Negativo",
      TRUE ~ "Neutro"
    )
  )

ggplot(
  vocabulario_lemma |> 
    filter(polaridade_lemma != "Neutro"),
  aes(
    x = reorder(lemma, sentimento_lemma),
    y = sentimento_lemma,
    fill = sentimento_lemma > 0
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
  words = vocabulario_lemma$lemma,
  freq = vocabulario_lemma$n,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  # ordered.colors = FALSE,
  colors = hcl.colors(
    n = nrow(vocabulario_lemma), 
    palette = "SunsetDark", rev = TRUE
  )
)

# Nuvem por TF-IDF médio
set.seed(2026)
wordcloud(
  words = vocabulario_lemma$lemma,
  freq = vocabulario_lemma$tfidf_medio * 100,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  # ordered.colors = FALSE,
  colors = hcl.colors(
    n = nrow(vocabulario_lemma), 
    palette = "SunsetDark", rev = TRUE
  )
)

## Nuvem de palavras baseada nas contribuições aos sentimentos ----
matriz_comparacao <- vocabulario_lemma |>
  select(lemma, polaridade_lemma, n) |>
  pivot_wider(
    names_from = polaridade_lemma,
    values_from = n,
    values_fill = 0,
    values_fn = sum
  ) |>
  column_to_rownames("lemma")

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
    "#A1C4E0",
    "#CC1A36"
  )
)

# Análise de emoções com léxicos ----------------------------------------------

## Identificação das categorias emocionais ----
tokens <- tokens |> 
  mutate(
    syuzhet::get_nrc_sentiment(
      char_v = lemma,
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
avaliacoes <- avaliacoes |> 
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

perfis_emocionais <- avaliacoes |>
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
