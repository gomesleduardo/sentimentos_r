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
# | - ANÁLISE DOS TRAILERS: GTA 6                   | #
# | https://gta.wiki/w/Grand_Theft_Auto_VI/Trailers | #
# --------------------------------------------------- #

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
library(rvest)
library(purrr)
library(patchwork)

# Corpus e documentos ---------------------------------------------------------

## Base de textos ----

# URL
url <- "https://gta.wiki/w/Grand_Theft_Auto_VI/Trailers"

# Leitura do HTML
pagina <- read_html(url)

# Linhas de transcrição do trailer 1
linhas_t1 <- pagina |>
  html_elements("table") |>
  pluck(4) |>
  html_elements(".nomobile > .scriptbox-line")

# Linhas de transcrição do trailer 2
linhas_t2 <- pagina |>
  html_elements("table") |>
  pluck(3) |>
  html_elements(".nomobile > .scriptbox-line")

# Transcrição do Trailer 1
transcript_trailer1 <- tibble(
  documento = linhas_t1 |>
    html_element(".scriptbox-line-time") |>
    html_text2(),
  texto = linhas_t1 |>
    html_element(".scriptbox-line-background") |>
    html_text2()
) |> 
  drop_na() |> 
  slice(2:(n() - 2))

# Transcrição do Trailer 2
transcript_trailer2 <- tibble(
  documento = linhas_t2 |>
    html_element(".scriptbox-line-time") |>
    html_text2(),
  texto = linhas_t2 |>
    html_element(".scriptbox-line-background") |>
    html_text2()
) |> 
  drop_na() |> 
  slice(2:(n() - 2))

rm(url, linhas_t1, linhas_t2, pagina)

# Tokenização -----------------------------------------------------------------

## Tokenização por palavras usando regex -----
tokens1 <- transcript_trailer1 |>
  unnest_tokens(
    output = token,
    input = texto,
    token = "regex",
    pattern = "[^[:alpha:]-]+"
  )

tokens2 <- transcript_trailer2 |>
  unnest_tokens(
    output = token,
    input = texto,
    token = "regex",
    pattern = "[^[:alpha:]-]+"
  )

# Normalização e pré-processamento --------------------------------------------

## Palavras frequentes e stopwords ----
stopwords_en <- get_stopwords(language = "en") |>
  rename(stopword = word) |>
  select(stopword)

## Lematização (via modelo UDpipe) ----

# Download e carregamento do modelo UDpipe
# modelo_lemat <- udpipe_download_model(
#   language = "english"
# )
# 
# modelo_en <- udpipe_load_model(
#   file = modelo_lemat$file_model
# )

modelo_en <- udpipe_load_model(
  file = 'english-ewt-ud-2.5-191206.udpipe'
)

tokens_lematizados1 <- udpipe_annotate(
  object = modelo_en,
  x = transcript_trailer1$texto,
  doc_id = as.character(transcript_trailer1$documento)
) |> 
  as_tibble() |> 
  select(doc_id, token, lemma, upos) |> 
  rename(documento = doc_id)

tokens_lematizados2 <- udpipe_annotate(
  object = modelo_en,
  x = transcript_trailer2$texto,
  doc_id = as.character(transcript_trailer2$documento)
) |> 
  as_tibble() |> 
  select(doc_id, token, lemma, upos) |> 
  rename(documento = doc_id)

# rm(modelo_lemat)
rm(modelo_en)

# Modificação necessária para a colar a lematização
tokens1 <- tokens1 |>
  group_by(documento, token) |>
  mutate(ocorrencia = row_number()) |>
  ungroup()

tokens_lematizados1 <- tokens_lematizados1 |>
  mutate(
    token = tolower(token)
  ) |> 
  group_by(documento, token) |>
  mutate(ocorrencia = row_number()) |>
  ungroup()

tokens2 <- tokens2 |>
  group_by(documento, token) |>
  mutate(ocorrencia = row_number()) |>
  ungroup()

tokens_lematizados2 <- tokens_lematizados2 |>
  mutate(
    token = tolower(token)
  ) |> 
  group_by(documento, token) |>
  mutate(ocorrencia = row_number()) |>
  ungroup()

# Unindo as informações
tokens1 <- tokens1 |>
  left_join(
    tokens_lematizados1,
    by = c("documento", "token", "ocorrencia")
  ) |>
  select(-ocorrencia) |> 
  drop_na(lemma)

tokens2 <- tokens2 |>
  left_join(
    tokens_lematizados2,
    by = c("documento", "token", "ocorrencia")
  ) |>
  select(-ocorrencia) |> 
  drop_na(lemma)

rm(tokens_lematizados1)
rm(tokens_lematizados2)

## Remoção de stopwords ----
tokens1 <- tokens1 |>
  anti_join(
    stopwords_en,
    by = join_by(lemma == stopword)
  ) |> 
  mutate(token = lemma) |> 
  select(- lemma)

tokens2 <- tokens2 |>
  anti_join(
    stopwords_en,
    by = join_by(lemma == stopword)
  ) |> 
  mutate(token = lemma) |> 
  select(- lemma)

# Análise de sentimentos com léxicos ------------------------------------------

## Construção de um léxico de sentimentos ----
lexico_nrc_en <- get_sentiment_dictionary(
  dictionary = "nrc", 
  language = "english"
)

## Escore por palavra ----
tokens1 <- tokens1 |> 
  mutate(
    sentimento = get_sentiment(
      token,
      method = "nrc",
      language = "english",
      lexicon = lexico_nrc_en
    )
  )

tokens2 <- tokens2 |> 
  mutate(
    sentimento = get_sentiment(
      token,
      method = "nrc",
      language = "english",
      lexicon = lexico_nrc_en
    )
  )

## Intensidade e polaridade por documento ----
transcript_trailer1 <- transcript_trailer1 |> 
  left_join(
    tokens1 |>
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

transcript_trailer2 <- transcript_trailer2 |> 
  left_join(
    tokens2 |>
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

# Análise de emoções com léxicos ----------------------------------------------

## Identificação das categorias emocionais ----
tokens1 <- tokens1 |> 
  mutate(
    syuzhet::get_nrc_sentiment(
      char_v = token,
      language = "english"
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

tokens2 <- tokens2 |> 
  mutate(
    syuzhet::get_nrc_sentiment(
      char_v = token,
      language = "english"
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
emocoes1 <- tokens1 |>
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

emocoes2 <- tokens2 |>
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
d1 <- ggplot(
  emocoes1,
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

d2 <- ggplot(
  emocoes2,
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

d1 <- d1 + labs(title = "Distribuição das categorias emocionais: Trailer 1")
d2 <- d2 + labs(title = "Distribuição das categorias emocionais: Trailer 2")

print(d1 / d2)

## Perfis emocionais por documento ----
transcript_trailer1 <- transcript_trailer1 |> 
  left_join(
    tokens1 |> 
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

transcript_trailer2 <- transcript_trailer2 |> 
  left_join(
    tokens2 |> 
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

perfis_emocionais1 <- transcript_trailer1 |>
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

perfis_emocionais2 <- transcript_trailer2 |>
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
p1 <- ggplot(
  perfis_emocionais1,
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
  theme_bw() + 
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    )
  )

p2 <- ggplot(
  perfis_emocionais2,
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
  theme_bw() + 
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    )
  )

p1 <- p1 + labs(title = "Perfis emocionais: Trailer 1")
p2 <- p2 + labs(title = "Perfis emocionais: Trailer 2")

print(p1 / p2)

## Proporções emocionais ----
g1 <- ggplot(
  perfis_emocionais1,
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
  theme_bw() + 
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    )
  )

g2 <- ggplot(
  perfis_emocionais2,
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
  theme_bw() + 
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    )
  )

g1 <- g1 + labs(title = "Composição relativa das emoções: Trailer 1")
g2 <- g2 + labs(title = "Composição relativa das emoções: Trailer 2")

print(g1 / g2)
