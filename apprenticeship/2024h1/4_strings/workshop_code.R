library(tidyverse)
library(dslabs)

View(reported_heights)

problems <- reported_heights |>
  mutate(new_height = as.numeric(height)) |>
  filter(is.na(new_height) | new_height < 50 | new_height > 84) |>
  pull(height) |>
  unique()

yes <- c("5'2", "6' 10\"", "5,10", "6.1")
no <- c("x", "2", "5.125", "123456")
s <- c(yes, no)
pattern <- "^\\s*([4-7])\\s*[',\\.\\s]\\s*(\\d{0,2})\\D*$"

str_detect(s, pattern)
str_match(s, pattern)
str_replace(yes, pattern, "\\1'\\2") |>
  str_split("'")

# feet: "^[4-7]$"
# cm: "^\\s*[12]\\d{2}\\D*$"
# feet and inches: "^\\s*[4-7]\\s*[',\\.\\s]\\s*\\d{0,2}\\D*$"

str_subset(problems, "^[4-7]$") |> unique()
str_subset(problems, "^\\s*[12]\\d{2}\\D*$") |> unique()
str_subset(problems, "^\\s*[4-7]\\s*[',\\.\\s]\\s*\\d{0,2}\\D*$") |> unique()

feet_pattern <- "^[4-7]$"
cm_pattern <- "^\\s*[12]\\d{2}\\D*$"
ft_in_pattern <- "^\\s*([4-7])\\s*([',\\.\\s]|ft|feet)\\s*(\\d{0,2})\\D*$"

heights <- reported_heights |>
  mutate(ft_in = str_replace(height, ft_in_pattern, "\\1'\\3"),
         new_height = as.numeric(height),
         new_height = case_when(
    str_detect(height, feet_pattern) ~ new_height*12,
    str_detect(height, cm_pattern) ~
      as.numeric(str_extract(height, "\\d{3}"))/2.54,
    str_detect(height, ft_in_pattern) ~
      as.numeric(str_extract(ft_in, "\\d(?=')"))*12 +
      as.numeric(str_extract(ft_in, "(?<=')\\d{0,2}")),
    TRUE ~ new_height
  )) |>
  select(-ft_in)

remaining_problems <- heights |>
  filter(is.na(new_height) | new_height < 50 | new_height > 84) |>
  pull(height) |>
  unique()
