library(rvest)
library(tidyverse)

url <- "https://en.wikipedia.org/wiki/List_of_liberal_arts_colleges_in_the_United_States"

# # import table from the Wikipedia url
# list <- read_html(url) |>
#   html_elements("#content li , a") |>
#   html_attr("href") |>
#   as.character() |>
#   as_tibble() |>
#   filter(str_detect(value, "href.*(University|College)"),
#          str_detect(value, "File", negate = TRUE)) |>
#   mutate(name = str_extract(value, "title = .*(?=(\"\\)))"),
#          name = str_remove(name, "title = \""),
#          url = str_extract(value, "href = \"/wiki/\\S*(?=\",)"),
#          url = str_replace(url, "href = \"", "https://en.wikipedia.org"))

html <- read_html(url) |>
  html_elements("#mw-content-text li a")

list <- tibble(url = html_attr(html, "href"),
               name = html_attr(html, "title")) |>
  filter(str_detect(name, "(University|College)")) |>
  mutate(url = paste0("https://en.wikipedia.org", url))


get_data <- function(data, name, url) {
  labels <- read_html(data$url) |>
    html_elements(".infobox-label") |>
    html_text2()

  values <- read_html(data$url) |>
    html_elements(".infobox-data") |>
    html_text2()

  tibble(name = data$name,
         url = data$url,
         variable = as.character(labels),
         value = as.character(values)) |>
    pivot_wider(names_from = variable, values_from = value) |>
    unlist()
}

data <- map_df(1:nrow(list), ~get_data(list[.x,], name, url))
data1 <- map_df(1:10, ~get_data(list[.x,], name, url))

colors <- data |>
  filter(str_detect(tolower(Type), "liberal arts")) |>
  select(name, Type, Undergraduates, Colors, Nickname, Mascot) |>
  mutate(known_colors = str_extract_all(tolower(Colors),
                                        paste(colors(), collapse = "|"))) |>
  unnest(known_colors) |>
  count(name, known_colors) |>
  filter(known_colors != "black" | n > 2) |>
  count(known_colors)

ggplot(colors, aes(n, fct_reorder(known_colors, n))) +
  geom_col(fill = colors$known_colors, color = "black") +
  xlab("Number of universities using a color") + ylab(NULL) +
  theme_light()
