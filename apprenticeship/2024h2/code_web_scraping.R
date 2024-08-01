library(tidyverse)
library(rvest)

url <- "https://raw.githack.com/ccs-amsterdam/r-course-material/master/miscellaneous/simple_html.html"
html <- read_html(url)

text <- html |>
  html_elements(".leftColumn p") |>
  html_text2()
cat(text)

html |>
  html_element("a") |>
  html_attrs()

html |>
  html_element("a") |>
  html_attr("href")


# -------------------------------------------------------------------------

url <- "https://en.wikipedia.org/wiki/Amherst_College"
html <- read_html(url)

label <- html |>
  html_elements(".infobox-label") |>
  html_text2()

value <- html |>
  html_elements(".infobox-data") |>
  html_text2()

infobox <- tibble(label, value)

r_colors <- colors() |> paste(collapse = "|")

infobox |>
  filter(label == "Colors") |>
  mutate(value = str_remove_all(value,
                                "\\.mw-parser-output.*\\.mw-parser-output"),
         value = str_to_lower(value),
         known_color = str_extract_all(value, r_colors)) |>
  unnest(known_color)

# -------------------------------------------------------------------------

url <- "https://en.wikipedia.org/wiki/List_of_liberal_arts_colleges_in_the_United_States"
html <- read_html(url) |>
  html_elements("#mw-content-text li a")

html |> html_attrs()

list <- tibble(
  url = html_attr(html, "href"),
  name = html_attr(html, "title")
) |>
  filter(str_detect(url, "^/wiki"),
         str_detect(name, "University|College")) |>
  mutate(url = paste0("https://en.wikipedia.org", url))

get_colors <- function(url) {
  html <- read_html(url)

  label <- html |>
    html_elements(".infobox-label") |>
    html_text2()

  value <- html |>
    html_elements(".infobox-data") |>
    html_text2()

  infobox <- tibble(url, label, value) |>
    filter(label == "Colors")

  infobox
}

get_colors("https://en.wikipedia.org/wiki/Amherst_College")

data <- map_df(list$url, get_colors)

colors <- data |>
  mutate(value = str_remove_all(value,
                                "\\.mw-parser-output.*\\.mw-parser-output"),
         value = str_to_lower(value),
         value = str_replace_all(value, "navy blue", "navy"),
         known_color = str_extract_all(value, r_colors)) |>
  unnest(known_color) |>
  distinct() |>
  left_join(distinct(list)) |>
  select(name, known_color)

color_counts <- colors |>
  count(known_color)

color_counts |>
  ggplot(aes(n, reorder(known_color, n))) +
  geom_col(fill = color_counts$known_color, color = "black")
