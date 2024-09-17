# load packages
library(tidyverse)
library(childesr)
library(qdapDictionaries)

# look at all the available corpora in the data set
corpora <- get_corpora()
View(corpora)

# download tokens from the Brown corpus
tokens <- get_tokens(token = "*", collection = "Eng-NA",
                     corpus = "Brown", role = "target_child")

# let's check the total number of words in the GradyAugmented vector
length(GradyAugmented)

# Create a new column 'is_valid' in tokens
valid_tokens <- tokens %>%
  mutate(is_valid = gloss %in% GradyAugmented)

# View the first few rows of gloss and is_valid to check the results
valid_tokens |>
  select(gloss, is_valid) |>
  head()

valid_tokens <- tokens |>
  mutate(is_valid = gloss %in% GradyAugmented,
         age = floor(target_child_age)) |>
  group_by(target_child_name, age) %>%
  summarise(valid_share = mean(is_valid))

valid_tokens |>
  ggplot(aes(x = age, y = valid_share, color = target_child_name)) +
  geom_line() +
  labs(title = "Development of Average Valid Word Usage Over Time",
       x = "Age (months)",
       y = "Average Valid Word Usage") +
  theme_minimal()

# create a vector with the provided list of common words
common_words <- c("the", "of", "to", "and", "a", "in", "is", "it", "you", "that", "he",
                  "was", "for", "on", "are", "with", "as", "I", "his", "they", "be",
                  "at", "one", "have", "this", "from", "or", "had", "by", "hot", "but",
                  "some", "what", "there", "we", "can", "out", "other", "were", "all",
                  "your", "when", "up", "use", "word", "how", "said", "an", "each",
                  "she", "which", "do", "their", "time", "if", "will", "way", "about",
                  "many", "then", "them", "would", "write", "like", "so", "these",
                  "her", "long", "make", "thing", "see", "him", "two", "has", "look",
                  "more", "day", "could", "go", "come", "did", "my", "sound", "no",
                  "most", "number", "who", "over", "know", "water", "than", "call",
                  "first", "people", "may", "down", "side", "been", "now", "find",
                  "any", "new", "work", "part", "take", "get", "place", "made", "live",
                  "where", "after", "back", "little", "only", "round", "man", "year",
                  "came", "show", "every", "good", "me", "give", "our", "under", "name",
                  "very", "through", "just", "form", "much", "great", "think", "say",
                  "help", "low", "line", "before", "turn", "cause", "same", "mean",
                  "differ", "move", "right", "boy", "old", "too", "does", "tell",
                  "sentence", "set", "three", "want", "air", "well", "also", "play",
                  "small", "end", "put", "home", "read", "hand", "port", "large",
                  "spell", "add", "even", "land", "here", "must", "big", "high", "such",
                  "follow", "act", "why", "ask", "men", "change", "went", "light",
                  "kind", "off", "need", "house", "picture", "try", "us", "again",
                  "animal", "point", "mother", "world", "near", "build", "self",
                  "earth", "father", "head", "stand", "own", "page", "should", "country",
                  "found", "answer", "school", "grow", "study", "still", "learn",
                  "plant", "cover", "food", "sun", "four", "thought", "let", "keep",
                  "eye", "never", "last", "door", "between", "city", "tree", "cross",
                  "since", "hard", "start", "might", "story", "saw", "far", "sea",
                  "draw", "left", "late", "run", "don't", "while", "press", "close",
                  "night", "real", "life", "few", "stop", "open", "seem", "together",
                  "next", "white", "children", "begin", "got", "walk", "example", "ease",
                  "paper", "often", "always", "music", "those", "both", "mark", "book",
                  "letter", "until", "mile", "river", "car", "feet", "care", "second",
                  "group", "carry", "took", "rain", "eat", "room", "friend", "began",
                  "idea", "fish", "mountain", "north", "once", "base", "hear", "horse",
                  "cut", "sure", "watch", "color", "face", "wood", "main", "enough",
                  "plain", "girl", "usual", "young", "ready", "above", "ever", "red",
                  "list", "though", "feel", "talk", "bird", "soon", "body", "dog",
                  "family", "direct", "pose", "leave", "song", "measure", "state",
                  "product", "black", "short", "numeral", "class", "wind", "question",
                  "happen", "complete", "ship", "area", "half", "rock", "order", "fire",
                  "south", "problem", "piece", "told", "knew", "pass", "farm", "top",
                  "whole", "king", "size", "heard", "best", "hour", "better", "true",
                  "during", "hundred", "am", "remember", "step", "early", "hold", "west",
                  "ground", "interest", "reach", "fast", "five", "sing", "listen", "six",
                  "table", "travel", "less", "morning", "ten", "simple", "several",
                  "vowel", "toward", "war", "lay", "against", "pattern", "slow", "center",
                  "love", "person", "money", "serve", "appear", "road", "map", "science",
                  "rule", "govern", "pull", "cold", "notice", "voice", "fall", "power",
                  "town", "fine", "certain", "fly", "unit", "lead", "cry", "dark",
                  "machine", "note", "wait", "plan", "figure", "star", "box", "noun",
                  "field", "rest", "correct", "able", "pound", "done", "beauty", "drive",
                  "stood", "contain", "front", "teach", "week", "final", "gave", "green",
                  "oh", "quick", "develop", "sleep", "warm", "free", "minute", "strong",
                  "special", "mind", "behind", "clear", "tail", "produce", "fact",
                  "street", "inch", "lot", "nothing", "course", "stay", "wheel", "full",
                  "force", "blue", "object", "decide", "surface", "deep", "moon", "island",
                  "foot", "yet", "busy", "test", "record", "boat", "common", "gold",
                  "possible", "plane", "age", "dry", "wonder", "laugh", "thousand", "ago",
                  "ran", "check", "game", "shape", "yes", "hot", "miss", "brought", "heat",
                  "snow", "bed", "bring", "sit", "perhaps", "fill", "east", "weight",
                  "language", "among")



# create the 'is_special' variable
special_tokens <- tokens |>
  mutate(is_valid = gloss %in% GradyAugmented,
         is_common = gloss %in% common_words,
         is_special = is_valid & !is_common)

# share of special words per month
special_tokens <- tokens |>
  mutate(is_valid = gloss %in% GradyAugmented,
         is_common = gloss %in% common_words,
         is_special = is_valid & !is_common,
         age = floor(target_child_age)) |>
  group_by(target_child_name, age) |>
  summarize(special_share = mean(is_special))

# create the plot
special_tokens |>
  ggplot(aes(x = age, y = special_share, color = target_child_name)) +
  geom_line() +
  labs(title = "Share of Special Words per Age Bracket",
       x = "Age (months)",
       y = "Share of Special Words") +
  theme_minimal()

# share of unique special words per month
unique_special_tokens <- tokens |>
  mutate(is_valid = gloss %in% GradyAugmented,
         is_common = gloss %in% common_words,
         is_special = is_valid & !is_common,
         age = floor(target_child_age)) |>
  # keep only unique tokens per child and age bracket
  distinct(target_child_name, age, gloss, is_special) |>
  group_by(target_child_name, age) |>
  summarize(special_share = mean(is_special))

# create the line plot
unique_special_tokens |>
  ggplot(aes(x = age, y = special_share, color = target_child_name)) +
  geom_line() +
  labs(title = "Share of Special Words per Age Bracket",
       x = "Age (months)",
       y = "Share of Special Words") +
  theme_minimal()
