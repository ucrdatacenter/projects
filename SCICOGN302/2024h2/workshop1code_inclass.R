# install.packages("tidyverse")
# install.packages("childesr")
# install.packages("tidytext")
library(tidyverse)
library(tidytext)
library(childesr)

utterances <- get_utterances(collection = "Eng-NA", corpus = c("VanKleeck", "Bates", "Bloom"))

utterances |>
  # filter(speaker_role == "Target_Child") |>
  group_by(speaker_role, target_child_age, target_child_sex) |>
  summarise(mlu = mean(num_tokens)) |>
  ggplot(aes(x = target_child_age, y = mlu, color = target_child_sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~speaker_role)


tokens <- get_tokens(collection = "Eng-NA", corpus = c("VanKleeck"), token = "*")

tokens |>
  filter(str_detect(part_of_speech, "^n$|^n:")) |>
  group_by(speaker_role, target_child_age, target_child_sex) |>
  summarise(nnouns = n_distinct(stem)) |>
  ggplot(aes(x = target_child_age, y = nnouns, color = target_child_sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~speaker_role)
