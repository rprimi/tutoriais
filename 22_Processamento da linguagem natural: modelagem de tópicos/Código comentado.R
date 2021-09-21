### NLP: topic modeling - um exemplo com Fernando Pessoa
### author: Alexandre Peres - alexandre.peres@gmail.com
### date: "04/02/2020"


### Instalando os pacotes do R
install.packages("ctv") #instala o pacote ctv para instalar o CRAN Task View de NLP
ctv::install.views("NaturalLanguageProcessing") #instala todos os pacotes de NLP no CRAN Task View
install.packages("tidyverse") #instala os pacotes do Tidyverse

### Abrindo o corpus no R
#Criamos uma pasta no nosso computador com os 49 documentos em formato `*.txt`.
#Criar o corpus com os 49 poemas do livro
library(tm)
corpus <- VCorpus(DirSource("C:/R/Pessoa/Alberto Caeiro/O Guardador de Rebanhos", #caminho da pasta com os arquivos
                            encoding = "UTF-8"), #codificação
                  readerControl = list(language = "pt")) #controle para leitura dos caracteres em português
corpus

### Preparando o texto
# Limpeza básica
library(tm)
corpus <- tm_map(corpus, stripWhitespace) #elimina espaços em branco duplicados
corpus <- tm_map(corpus, removePunctuation) #remove pontuações
corpus <- tm_map(corpus, removeNumbers) #remove números
corpus <- tm_map(corpus, content_transformer(tolower)) #transforma letras maiúsculas em minúsculas
library(tm)
corpus <- tm_map(corpus, removeWords, stopwords("portuguese")) 
# Desfolhando as palavras (stemming)
library(tm)
corpus <- tm_map(corpus, stemDocument)

### Transformandos os textos em uma matriz
library(tm)
dtm <- DocumentTermMatrix(corpus) #criar um matriz documento-termo
inspect(dtm)

# Removendo termos esparsos
library(tm)
dtm <- removeSparseTerms(dtm, 0.95)
inspect(dtm)

### Analisando a frequência das palavras
library(tm)
tm::findFreqTerms(dtm, lowfreq = 15)
tm::findMostFreqTerms(dtm, n=5)

### Usando o tidyverse 
library(dplyr)
library(tibble)
library(tidytext)
tidy.text <- tidy(corpus) %>%
  unnest_tokens(termos, text) %>%
  count(id, termos, sort = TRUE) %>%
  arrange(desc(n))
tidy.text

#Removendo as palavras de parada com o tidyverse
palavras_parada <- read.delim("~/Meus artigos/NLP Topic Modeling/palavras_parada.txt",
                              encoding="UTF-8", header = FALSE)
palavras_parada <- as.character(palavras_parada$V1)
tidy.text <- tidy.text %>% filter(!termos %in% palavras_parada)
tidy.text

#Transformar a tdm em uma tibble
dtm <- tidy(dtm)
dtm <- dtm %>% arrange(desc(count))
dtm

#Analisar a frequência dos termos e TF-IDF
#Contagem dos termos por documento e nuvem de palavras
library(wordcloud)
tidy.text %>% count(termos) %>%
  with(wordcloud(termos, n, max.words = 100, scale = c(2, 0.75)))

#Calcular TF-IDF
tidy.text <- tidy.text %>%
  bind_tf_idf(termos, id, n)
tidy.text

#plotar histograma dos 10 termos com maior n
library(ggplot2)
top.n.plot <- tidy.text %>%
  arrange(desc(n)) %>%
  mutate(termos = factor(termos, unique(termos))) %>%
  top_n(10) %>%
  ggplot(aes(termos, n)) +
  geom_col(color="#619CFF", fill="#619CFF", show.legend = FALSE) +
  labs(x=NULL, y="n") +
  coord_flip()

#plotar histograma dos 10 termos com maior tf
top.tf.plot <- tidy.text %>%
  arrange(desc(tf)) %>%
  mutate(termos = factor(termos, unique(termos))) %>%
  top_n(10) %>%
  ggplot(aes(termos, tf)) +
  geom_col(color="#E58700", fill="#E58700", show.legend = FALSE) +
  labs(x=NULL, y="TF") +
  coord_flip()

#plotar histograma dos 10 termos com maior TF-IDF
top.TFIDF.plot <- tidy.text %>%
  arrange(desc(tf_idf)) %>%
  mutate(termos = factor(termos, unique(termos))) %>%
  top_n(10) %>%
  ggplot(aes(termos, tf_idf)) +
  geom_col(color="#00C0AF", fill="#00C0AF", show.legend = FALSE) +
  labs(x=NULL, y="TF-IDF") +
  coord_flip()

#plotar os 3 graficos em um painel com 3 colunas
library(ggpubr)
ggarrange(top.n.plot, top.tf.plot, top.TFIDF.plot, ncol = 3, nrow = 1)


### Analisando a relação entre os termos
library(widyr)
pares.termos <- tidy.text %>% 
  pairwise_count(termos, id, sort = TRUE, upper = FALSE)
pares.termos

corr.termos <- tidy.text %>% 
  group_by(termos) %>%
  filter(n() >= 3) %>%
  pairwise_cor(termos, id, sort = TRUE, upper = FALSE)
corr.termos

#network das co-ocorrências entre os termos
library(ggplot2)
library(igraph)
library(ggraph)
set.seed(1234)
pares.termos %>%
  filter(n >= 5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "#F8766D") +
  geom_node_point(size = 3) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()

#network das correlações entre os termos
set.seed(1234)
corr.termos %>%
  filter(correlation > abs(.7)) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "#619CFF") +
  geom_node_point(size = 3) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  labs(title = "", caption = "")
theme_void()

### Decidindo sobre ao quantidade de tópicos a serem gerados

#Converter a tiblle em dtm, formato com o qual o pacote ldatuning trabalha
dtm <- tidy.text %>% cast_dtm(id, termos, n)
#Explorar soluções com 2 a 50 tópicos com o ldatuning
library(ldatuning)
n.topicos <- FindTopicsNumber(dtm = dtm, topics = seq(from = 2, to = 50, by = 1),
                              metrics = c("Griffiths2004", "CaoJuan2009",
                                          "Arun2010", "Deveaud2014"),
                              method = "Gibbs", control = list(seed = 1),
                              mc.cores = 3, verbose = TRUE)
ntopics <- FindTopicsNumber_plot(n.topicos) #plotar gráfico com resultados
#Explorar soluções com 2 a 50 tópicos com a métrica perplexity
library(purrr)
library(topicmodels)
k_topics <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 25, 30, 35, 40, 45, 50)
modelos.lda <- k_topics %>% map(LDA, x = dtm, control = list(seed = 1234))
perp <- tibble(k = k_topics, perplexidade = map_dbl(modelos.lda, perplexity)) %>%
  ggplot(aes(k, perplexidade)) +
  geom_point() + geom_line() +
  labs(y="Perplexidade", x="Quantidade de tópicos")
library(ggpubr)
ggarrange(perp, ntopics, ncol = 1, nrow = 2)


###Gerar modelo LDiA com dois tópicos
library(topicmodels)
LDiA <- LDA(dtm, k=2, method = "Gibbs", control = list(seed=123))
LDiA
#gerar tibble com distribuição dos termos por tópicos
topicos <- tidy(LDiA, matrix = "beta")
topicos
#top termos por tópico
top20termos <-  topicos %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#gráfico de colunas com os termos de maior probabilidade em cada tópico
library(ggplot2)
top20termos %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(y= "Beta", x = "Termos") +
  facet_wrap(~topic, scales="free") +
  coord_flip() +
  scale_x_reordered()

#Termos que mais diferenciam os dois tópicos
library(tidyr)
topico1.topico2 <- topicos %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))
topico1.topico2
#gráfico de barras (diverging bar) com esses termos 
library(ggplot2)
topico1.topico2 %>%
  arrange(desc(abs(log_ratio))) %>%
  head(30) %>%
  ggplot(aes(x = reorder(term, log_ratio), y = log_ratio,
             fill = log_ratio > 0)) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(name="Tópicos", labels=c("Tópico 1", "Tópico 2")) +
  labs(y= "Razão de log: log2(beta_topico2/beta_topico1)", x = "Termos") +
  coord_flip()

#Gerar tibble com as probabilidades de cada documento pertencer a cada tópico
doc.topicos <- tidy(LDiA, matrix = "gamma")
#reduzir o título do poema para ficar apenas o algarismo romano
library(stringr)
doc.topicos$document <- word(doc.topicos$document, 1)
#gráfico indicando as probabilidades dos poemas pertencerem a cada tópico
library(ggplot2)
doc.topicos %>%
  mutate(title = reorder(document, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma, fill=topic)) +
  scale_fill_continuous(low="deepskyblue4", high="firebrick") +
  geom_col() +
  labs(y= "Gama", x = "Tópicos") +
  theme(legend.position = "none") +
  facet_wrap(~ title, ncol = 7)