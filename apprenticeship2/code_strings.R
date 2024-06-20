library(tidyverse)
library(dslabs)
library(english)

View(reported_heights)

problems <- reported_heights |>
  mutate(height_num = as.numeric(height)) |>
  filter(is.na(height_num) | height_num < 50 | height_num > 84) |>
  pull(height) |>
  unique()

yes <- c("5' 4\"", "5'7", "5'10''", "5.9")
no <- c("1", "11111", "12.3")
s <- c(yes, no)

pattern <- "\\d'\\d"
str_detect(s, pattern)
str_view(s, pattern)

cm_pattern <- "^[1-2]\\d{2}\\s*(cm)?$"
ft_pattern <- "^[4-7]\\D*$"
ft_in_pattern <- "^([4-7])(\\s*[',\\.\\s+]\\s*)([\\d\\.]+)\\D*$"

str_subset(problems, ft_in_pattern)

str_match(problems, ft_in_pattern)

problems |>
  str_remove_all("inches|and") |>
  str_replace_all("feet|ft|foot", "'") |>
  str_subset(cm_pattern, negate = TRUE) |>
  str_subset(ft_pattern, negate = TRUE) |>
  str_subset(ft_in_pattern, negate = TRUE)

ft_in_sep <- c(feet = "[4-7]", "\\s*'\\s*", inches = "[\\d\\.]+", "\\D*")

numbers <- tibble(
  number = 1:12,
  word = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve")
)

data <- reported_heights |>
  mutate(new_height = height |>
           str_to_lower() |>
           str_remove_all("inches|and|cm") |>
           str_replace_all(ft_in_pattern, "\\1'\\3") |>
           str_replace_all("feet|ft|foot", "'")) |>
  separate(new_height, into = c("feet_word", "inches_word"), remove = FALSE) |>
  left_join(numbers, by = c("feet_word" = "word")) |>
  left_join(numbers, by = c("inches_word" = "word")) |>
  mutate(new_height = ifelse(!is.na(number.x),
                             paste(number.x, number.y, sep = "'"),
                             new_height),
         ft_in = ifelse(str_detect(new_height, ft_in_pattern),
                        new_height,
                        NA)) |>
  separate_wider_regex(ft_in, pattern = ft_in_sep) |>
  mutate(feet = as.numeric(feet),
         inches = as.numeric(inches),
         height_num = as.numeric(new_height),
         height_num = case_when(
           str_detect(new_height, ft_pattern) ~ parse_number(new_height) * 12,
           str_detect(new_height, cm_pattern) ~ parse_number(new_height) / 2.54,
           !is.na(feet) ~ feet * 12 + inches,
           TRUE ~ height_num
         ))

data |>
  filter(is.na(height_num) | height_num < 50 | height_num > 84) |>
  pull(height) |>
  unique()

paste0("^", paste0(numbers$word, "x", collapse = "|"), "$")
