library(tidyverse)
library(tidytext)
library(jsonlite)
library(widyr)
library(igraph)
library(ggraph)

metadata <- fromJSON("data.json")
df <- metadata[[6]] |>
  as_tibble() |>
  select(title) |>
  rename(word = title) |>
  mutate(id = row_number()) |>
  # unnest(word) |>
  unnest_tokens(word, word) |>
  anti_join(stop_words) |>
  mutate(word = toupper(word)) |>
  filter(!str_detect(word,"\\d"))

df |>
  count(word, sort = T) |>
  slice_head(n = 25) |>
  ggplot(aes(reorder(word,n),n)) + geom_col() + coord_flip()

df |>
  # group_by(word) |>
  # filter(n() >= 100) |>
  # pairwise_cor(word, id, sort = TRUE, upper = FALSE) |>
  pairwise_count(word, id, sort = TRUE, upper = FALSE) |>
  # filter(correlation > 0.6) |>
  filter(n > 200) |>
  graph_from_data_frame() |>
  ggraph(layout = "fr") +
  # geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

df |>
  count(id, word, sort = TRUE) |>
  bind_tf_idf(word, id, n)
