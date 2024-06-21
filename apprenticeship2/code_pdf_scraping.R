library(tidyverse)
library(pdftools)

pdf <- pdf_text("https://www.europarl.europa.eu/meps/en/full-list/pdf")

pdf[1]
cat(pdf[1])

meps <- pdf |>
  paste(collapse = "\n\n") |>
  str_split_1(pattern = "\n\n") |>
  tibble(name = _) |>
  mutate(name = str_remove(name, "^Members"),
         name = str_remove(name, "^\n")) |>
  filter(str_detect(name, "^\\s")) |>
  mutate(name = str_trim(name)) |>
  separate(name, c("name", "group", "country"), sep = "\n") |>
  mutate(across(everything(), str_trim)) |>
  separate(country, into = c("country", "party"), sep = " ", extra = "merge")

View(meps)

meps |>
  count(group, country) |>
  ggplot(aes(n, country, fill = group)) +
  geom_col()
