# 📊 Análise de Sentimentos em R

### Minicurso | 10ª Semana de Iniciação Científica (SEMIC 2026) — ENCE/IBGE

![R](https://img.shields.io/badge/R-Statistical%20Computing-276DC3?logo=r&logoColor=white)
![tidyverse](https://img.shields.io/badge/tidyverse-Data%20Science-1A162D)
![NLP](https://img.shields.io/badge/NLP-Processamento%20de%20Linguagem%20Natural-blueviolet)
![License](https://img.shields.io/badge/uso-educacional-green)

## 📌 Sobre o minicurso

A **Análise de Sentimentos** é uma área do Processamento de Linguagem Natural (*Natural Language Processing* — NLP) dedicada à identificação e à quantificação de opiniões, sentimentos e emoções expressos em textos.

Este repositório reúne os códigos e materiais utilizados no minicurso **Análise de Sentimentos em R**, realizado durante a **10ª Semana de Iniciação Científica (SEMIC 2026)** da Escola Nacional de Ciências Estatísticas (ENCE/IBGE).

O minicurso apresenta uma introdução teórica e prática às principais técnicas de análise de sentimentos baseadas em léxicos, utilizando a linguagem R.

Ao longo das atividades, são exploradas diferentes estratégias para transformar textos não estruturados em informações quantitativas, identificar polaridades positivas e negativas, reconhecer emoções e visualizar padrões linguísticos.

A proposta é combinar os fundamentos do Processamento de Linguagem Natural com uma perspectiva estatística e exploratória da análise textual.

## 🎯 Objetivos

Ao final do minicurso, espera-se que os participantes sejam capazes de:

- Compreender os conceitos fundamentais de Processamento de Linguagem Natural e Análise de Sentimentos.
- Organizar e preparar bases de dados textuais utilizando R.
- Realizar tokenização, limpeza e pré-processamento de textos.
- Construir representações quantitativas de documentos.
- Aplicar métodos de análise de sentimentos baseados em léxicos.
- Identificar emoções e polaridades expressas em textos.
- Explorar a evolução dos sentimentos ao longo de documentos.
- Produzir visualizações gráficas para interpretar os resultados.
- Reconhecer as limitações e os desafios metodológicos da análise automatizada de sentimentos.

## 📚 Conteúdo programático

### 1. Introdução ao Processamento de Linguagem Natural

- O que é Processamento de Linguagem Natural?
- Principais aplicações.
- Textos como dados.
- Conceitos fundamentais de Análise de Sentimentos.
- Abordagens baseadas em léxicos e métodos computacionais.

### 2. Análise textual com `tidytext`

O pacote `tidytext` permite integrar técnicas de mineração de textos ao ecossistema `tidyverse`.

**Principais tópicos:**

- Organização de dados textuais.
- Tokenização.
- Remoção de *stopwords*.
- Pré-processamento e normalização.
- Frequência de palavras.
- Representação termo-documento.
- TF-IDF (*Term Frequency–Inverse Document Frequency*).
- Léxicos de sentimentos.
- Visualizações e nuvens de palavras.

### 3. Análise de sentimentos com `syuzhet`

O pacote `syuzhet` oferece ferramentas para quantificar sentimentos e emoções em textos.

**Principais tópicos:**

- Métodos de análise lexical.
- Cálculo de escores de sentimentos.
- Léxicos disponíveis.
- Identificação de emoções com o léxico NRC.
- Perfis emocionais de documentos.
- Evolução temporal dos sentimentos.
- Visualização dos resultados.

O léxico NRC permite explorar oito categorias emocionais:

| Emoção | Emoção | Emoção | Emoção |
|---|---|---|---|
| 😠 Raiva | 🔮 Antecipação | 🤢 Nojo | 😨 Medo |
| 😄 Alegria | 😢 Tristeza | 😲 Surpresa | 🤝 Confiança |

### 4. Análise contextual com `sentimentr`

Diferentemente das abordagens que consideram palavras isoladamente, o pacote `sentimentr` incorpora informações contextuais ao cálculo dos sentimentos.

**Principais tópicos:**

- Fundamentos da análise contextual.
- Polaridade de sentenças.
- Negadores e intensificadores.
- Modificadores de valência.
- Análise de sentimentos por sentença e documento.
- Visualização e interpretação dos resultados.

Por exemplo, considere:

> "O atendimento foi bom."

> "O atendimento não foi bom."

Embora ambas as sentenças contenham a palavra *bom*, seus significados são diferentes. Esse exemplo ilustra a importância de considerar o contexto linguístico.

### 5. Considerações finais

- Comparação entre as abordagens apresentadas.
- Possibilidades de integração entre pacotes.
- Limitações dos métodos baseados em léxicos.
- Desafios da análise de textos em português.
- Perspectivas para aplicações e pesquisas futuras.

## 🛠️ Tecnologias utilizadas

O minicurso utiliza a linguagem **R** e os seguintes pacotes:

| Pacote | Finalidade |
|---|---|
| `tidyverse` | Manipulação, transformação e visualização de dados |
| `tidytext` | Mineração e organização de dados textuais |
| `stringi` | Manipulação e normalização de strings |
| `wordcloud` | Construção de nuvens de palavras |
| `tm` | Ferramentas de mineração de textos |
| `udpipe` | Processamento e anotação linguística |
| `syuzhet` | Análise de sentimentos e emoções |
| `sentimentr` | Análise contextual de polaridade |

## 🚀 Como começar

### 1. Instale o R e o RStudio

- **R:** https://cran.r-project.org/
- **RStudio:** https://posit.co/download/rstudio-desktop/

Recomenda-se instalar primeiro o R e, em seguida, o RStudio.

### 2. Obtenha os arquivos do repositório

Você pode baixar o conteúdo pelo botão **Code → Download ZIP**, na página do GitHub.

Alternativamente, utilize o Git:

```bash
git clone https://github.com/gomesleduardo/sentimentos_r.git
```

### 3. Instale os pacotes necessários

Execute no console do R:

```r
install.packages(c(
  "tidyverse",
  "tidytext",
  "stringi",
  "wordcloud",
  "tm",
  "udpipe",
  "syuzhet",
  "sentimentr"
))
```

### 4. Carregue os pacotes

```r
library(tidyverse)
library(tidytext)
library(stringi)
library(wordcloud)
library(tm)
library(udpipe)
library(syuzhet)
library(sentimentr)
```

Agora você já pode explorar os exemplos e acompanhar as atividades práticas.

## 💻 Exemplo prático

Vamos construir uma pequena base de avaliações de atendimento:

```r
library(tidyverse)
library(tidytext)

avaliacoes <- tibble(
  documento = 1:4,
  texto = c(
    "O atendimento foi excelente e rápido!",
    "O serviço foi muito bom.",
    "O atendimento foi péssimo e demorado.",
    "O atendimento não foi bom!"
  )
)
```

Em seguida, podemos transformar os textos em uma estrutura organizada por palavras:

```r
palavras <- avaliacoes |>
  unnest_tokens(
    output = palavra,
    input = texto
  )

palavras
```

Essa transformação é uma das etapas fundamentais da mineração de textos, pois permite aplicar procedimentos de contagem, classificação, associação com léxicos e visualização.

**Observação:** a tokenização, isoladamente, não determina a polaridade das avaliações. Para isso, é necessário aplicar métodos ou léxicos adequados ao idioma e ao contexto analisado.

## 📈 Aplicações

As técnicas apresentadas podem ser utilizadas em diferentes contextos, como:

- Avaliação da satisfação de usuários de serviços.
- Análise de opiniões sobre produtos e organizações.
- Monitoramento de comentários em redes sociais.
- Investigação de percepções sobre políticas públicas.
- Análise exploratória de respostas abertas em questionários.
- Estudos de comunicação e comportamento.
- Pesquisas acadêmicas envolvendo dados textuais.

## ⚠️ Limitações importantes

A Análise de Sentimentos automatizada apresenta desafios que devem ser considerados na interpretação dos resultados.

Entre eles, destacam-se:

- Ambiguidade e dependência do contexto.
- Ironia e sarcasmo.
- Expressões idiomáticas.
- Negação e intensificação.
- Diferenças culturais e linguísticas.
- Cobertura limitada de alguns léxicos em português.
- Diferenças entre sentimentos expressos e sentimentos efetivamente experimentados.

**Os escores de sentimentos são medidas construídas a partir de procedimentos computacionais, e não observações diretas de estados emocionais.**

Portanto, sua interpretação exige atenção aos pressupostos metodológicos e às características dos dados analisados.

## 📖 Referências

- Silge, J., & Robinson, D. (2017). *Text Mining with R: A Tidy Approach*. O'Reilly Media. https://www.tidytextmining.com/

- Liu, B. (2012). *Sentiment Analysis and Opinion Mining*. Morgan & Claypool Publishers. https://doi.org/10.2200/S00416ED1V01Y201204HLT016

- Mohammad, S. M., & Turney, P. D. (2013). Crowdsourcing a Word–Emotion Association Lexicon. *Computational Intelligence, 29*(3), 436–465.

- Hutto, C. J., & Gilbert, E. (2014). VADER: A Parsimonious Rule-Based Model for Sentiment Analysis of Social Media Text. *Proceedings of ICWSM*, 216–225.

- Jurafsky, D., & Martin, J. H. *Speech and Language Processing*. https://web.stanford.edu/~jurafsky/slp3/

## 🎓 Informações do minicurso

**Título:** Análise de Sentimentos em R

**Evento:** 10ª Semana de Iniciação Científica (SEMIC 2026)

**Instituição:** Escola Nacional de Ciências Estatísticas (ENCE) — Instituto Brasileiro de Geografia e Estatística (IBGE)

**Carga horária:** 3 horas

**Formato:** Teórico-prático

**Repositório:** https://github.com/gomesleduardo/sentimentos_r

---

### ⭐ Gostou do material?

Se este repositório foi útil para seus estudos ou pesquisas, considere deixar uma **⭐ estrela** no GitHub.

Os materiais têm finalidade educacional e podem servir como ponto de partida para estudos e aplicações de Análise de Sentimentos em R.

*Bons estudos e boas análises! 📊*
