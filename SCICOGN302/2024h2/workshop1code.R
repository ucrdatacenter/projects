library(tidyverse)
library(tidytext)
library(childesr)

tokens <- get_tokens(token = "*", collection = "Eng-NA", target_child = "Amy",
                     corpus = "VanKleeck", role = "target_child")

utterances <- get_utterances(collection = "Eng-NA", target_child = "Amy",
                             corpus = "VanKleeck", role = "target_child")

n_tokens <- count(tokens, stem, sort = TRUE)
head(n_tokens, 10)

tokens_filtered <- filter(tokens, !(stem %in% stop_words$word) & stem != "")

n_filtered <- count(tokens_filtered, stem, sort = TRUE)
head(n_filtered, 10)

n_filtered <- tokens %>%
  filter(!(stem %in% stop_words$word) & stem != "") %>%
  count(stem, sort = TRUE)

utterances %>%
  filter(str_detect(gloss, "he is"))

n_filtered %>%
  head(10) %>%
  ggplot() +
  geom_col(aes(x = n, y = reorder(stem, n))) +
  labs(x = "Frequency", y = "")

utterances %>%
  ggplot(aes(x = utterance_order, y = num_tokens)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Time in conversation", y = "Mean length of utterance") +
  theme_light()

utterances %>%
  ggplot(aes(x = utterance_order, y = num_tokens,
             color = as.character(transcript_id))) +
  geom_point() +
  geom_smooth() +
  labs(x = "Time in conversation", y = "Mean length of utterance") +
  theme_light()

tokens %>%
  filter(str_detect(part_of_speech, "pro:")) %>%
  count(part_of_speech) %>%
  ggplot() +
  geom_col(aes(x = part_of_speech, y = n)) +
  labs(x = "Pronoun type", y = "Frequency") +
  theme_light()
