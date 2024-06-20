library(tidyverse)
library(tidytext)
library(gutenbergr)
library(ggwordcloud)
library(igraph)
library(ggraph)
library(topicmodels)
library(word2vec)

# Download books based on their Gutenberg ID
# https://gutenberg.org/ebooks/19033
# https://gutenberg.org/ebooks/67098
books <- gutenberg_download(c(19033, 67098)) |>
  # add book title
  mutate(book_title = ifelse(gutenberg_id == 19033, "Alice", "Winnie")) |>
  # restart counting row numbers for each book
  group_by(book_title) |>
  filter((book_title == "Alice" & row_number() >= 38) |
           (book_title == "Winnie" & row_number() >= 79)) |>
  ungroup()

words <- books |>
  unnest_tokens(output = word, input = text) |>
  mutate(word = str_remove_all(word, "_")) |>
  filter(!str_detect(word, "^\\d+$"))

tf <- words |>
  count(book_title, word) |>
  group_by(book_title) |>
  mutate(tf = n / sum(n))

tf_idf <- words |>
  count(book_title, word) |>
  bind_tf_idf(word, book_title, n)

dtm <- words |>
  count(book_title, word) |>
  pivot_wider(names_from = word, values_from = n, values_fill = 0)

dtm <- words |>
  count(book_title, word) |>
  cast_dtm(book_title, word, n)


tf_idf |>
  slice_max(tf_idf, n = 10, by = book_title) |>
  ggplot(aes(tf_idf, reorder_within(word, tf_idf, book_title))) +
  geom_col() +
  scale_y_reordered() +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~ book_title, scales = "free") +
  theme_minimal()

stopwords <- get_stopwords() |> pull(word)

set.seed(1)
tf_idf |>
  filter(!word %in% stopwords) |>
  slice_max(tf, n = 20, by = book_title) |>
  ggplot(aes(label = word, size = tf)) +
  geom_text_wordcloud(shape = "square") +
  facet_wrap(~ book_title, scales = "free") +
  theme_void()

bigrams <- books |>
  unnest_tokens(output = bigram, input = text, token = "ngrams", n = 2) |>
  drop_na()

bigrams |>
  count(book_title, bigram) |>
  bind_tf_idf(bigram, book_title, n) |>
  separate(bigram, into = c("word1", "word2"),
           sep = " ", remove = FALSE) |>
  filter(!word1 %in% stopwords, !word2 %in% stopwords) |>
  slice_max(tf, n = 10, by = book_title) |>
  ggplot(aes(tf, reorder_within(bigram, tf, book_title))) +
  geom_col() +
  scale_y_reordered() +
  labs(x = "tf", y = NULL) +
  facet_wrap(~ book_title, scales = "free") +
  theme_minimal()

# Creating an igraph graph from the data frame
g_alice <- bigrams |>
  filter(book_title == "Alice") |>
  separate(bigram, into = c("word1", "word2"), sep = " ") |>
  count(word1, word2) |>
  filter(n > 2) |>
  filter(!word1 %in% stopwords, !word2 %in% stopwords) |>
  graph_from_data_frame(directed = TRUE)

# Plotting the graph using ggraph
ggraph(g_alice, layout = "fr") +
  geom_edge_link(aes(width = n), alpha = 1,
                 arrow = arrow(length = unit(0.2, "cm"))) +
  geom_node_point(size = 1, color = "lightblue") +
  geom_node_text(aes(label = name)) +
  scale_edge_width(range = c(0.1, 1)) +
  theme_void()

# Creating an igraph graph from the data frame
g_winnie <- bigrams |>
  filter(book_title == "Winnie") |>
  separate(bigram, into = c("word1", "word2"), sep = " ") |>
  count(word1, word2) |>
  filter(n > 5) |>
  filter(!word1 %in% stopwords, !word2 %in% stopwords) |>
  graph_from_data_frame(directed = TRUE)

# Plotting the graph using ggraph
ggraph(g_winnie, layout = "fr") +
  geom_edge_link(aes(width = n), alpha = 1,
                 arrow = arrow(length = unit(0.2, "cm"))) +
  geom_node_point(size = 1, color = "lightblue") +
  geom_node_text(aes(label = name)) +
  scale_edge_width(range = c(0.1, 1)) +
  theme_void()

bing <- get_sentiments("bing")
afinn <- get_sentiments("afinn")

bing_books <- tf_idf |>
  inner_join(bing, by = "word") |>
  slice_max(tf, n = 10, by = book_title)
bing_books |>
  ggplot(aes(tf, reorder_within(word, tf, book_title), fill = sentiment)) +
  geom_col() +
  scale_y_reordered() +
  labs(x = "tf", y = NULL) +
  facet_wrap(~ book_title, scales = "free") +
  theme_minimal()

tf_idf |>
  inner_join(afinn, by = "word") |>
  mutate(score = n * value) |>
  group_by(book_title) |>
  summarize(score = sum(score) / sum(n))

dtm <- words |>
  filter(!word %in% stopwords) |>
  count(book_title, word) |>
  cast_dtm(book_title, word, n)

lda <- LDA(dtm, k = 2, control = list(seed = 4))

topic_prob <- tidy(lda, matrix = "beta")
topic_prob |>
  slice_max(beta, n = 5, by = topic)

tidy(lda, matrix = "gamma")

text <- words |>
  group_by(book_title) |>
  summarize(text = paste(text, collapse = " ")) |>
  pull(text)

embeddings <- word2vec(text, dim = 50)

predict(embeddings, unique(words$word), type = "embedding")
predict(embeddings, "alice", type = "nearest", top_n = 5)
