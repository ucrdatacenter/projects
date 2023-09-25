# install.packages("tidyverse")
# install.packages("tidytext")
# install.packages("childesr")

library(tidyverse)
library(tidytext)
library(childesr)

# load data
tok <- get_tokens(token = "*", collection = "Eng-NA", target_child = "Amy",
                  corpus = "VanKleeck", role = "target_child")

utt <- get_utterances(collection = "Eng-NA", target_child = "Amy",
                      corpus = "VanKleeck", role = "target_child")

# most frequent words in unfiltered data
n <- count(tok, stem, sort = TRUE)
head(n, 10)

# most frequent words in unfiltered data
tok_filtered <- filter(tok, !(stem %in% stop_words$word) & stem != "")
n_filtered <- count(tok_filtered, stem, sort = TRUE)
head(n_filtered, 10)

# most frequent words in unfiltered data - pipe workflow
n_filtered <- tok %>%
  filter(!(stem %in% stop_words$word) & stem != "") %>%
  count(stem, sort = TRUE)

# regex filter with grepl()
utt %>%
  filter(grepl("he is", gloss))

# bar chart of most frequent words
n_filtered %>%
  head(10) %>%
  ggplot() +
  geom_col(aes(x = n, y = reorder(stem, n))) +
  labs(x = "Frequency", y = "")

# MLU over time
utt %>%
  ggplot(aes(x = utterance_order, y = num_tokens)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Time in conversation", y = "Mean length of utterance") +
  theme_light()

# MLU over time, per transcript
utt %>%
  ggplot(aes(x = utterance_order, y = num_tokens,
             color = as.character(transcript_id))) +
  geom_point() +
  geom_smooth() +
  labs(x = "Time in conversation", y = "Mean length of utterance") +
  theme_light()

# Bar chart of pronoun types
tok %>%
  filter(grepl("pro:", part_of_speech)) %>%
  count(part_of_speech) %>%
  ggplot() +
  geom_col(aes(x = part_of_speech, y = n)) +
  labs(x = "Pronoun type", y = "Frequency") +
  theme_light()
