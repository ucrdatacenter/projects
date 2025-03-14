library(tidyverse)
library(dslabs)

View(reported_heights)

problems <- reported_heights |> 
  mutate(new_height = as.numeric(height)) |> 
  filter(is.na(new_height) | new_height < 50 | new_height > 84) |> 
  pull(height) |> 
  unique()

# practice with regular expressions

pattern <- "\\s*(feet|ft|inches)\\s*"

str_view(problems, pattern)
str_subset(problems, pattern)

problems |> 
  str_subset(cm_pattern, negate = TRUE) |> 
  str_subset(m_pattern, negate = TRUE) |> 
  str_subset(ft_pattern, negate = TRUE) |> 
  str_replace("\\s*(foot|feet|ft)\\s*(and\\s*)?", "'") |> 
  str_remove("\\s*(inches|\"|'')\\s*$") |> 
  str_replace("([4-7])(\\s*[\\.,\\s+']\\s*)(\\d{1,2})", "\\1'\\3")
  # str_match("([4-7])(\\s*[\\.,\\s+']\\s*)(\\d{1,2})")

# fixing all problems in reported_heights

cm_pattern <- "^\\s*[12]\\d{2}(\\.\\d+)?\\s*(cm)?$"
m_pattern <- "^\\s*[12][\\.,]\\d+\\s*$"
ft_pattern <- "^\\s*[5-7]'?\\s*$"
ft_in_pattern <- c(feet = "[4-7]", "\\s*[\\.,\\s+']\\s*", inches = "\\d{1,2}\\.?\\d*")

fixed_heights <- reported_heights |> 
  mutate(height = height |> 
           str_replace("\\s*(foot|feet|ft)\\s*(and\\s*)?", "'") |> 
           str_remove("\\s*(inches|\"|''|cm)\\s*$"),
         new_height = as.numeric(height)) |> 
  separate_wider_regex(height, patterns = ft_in_pattern, cols_remove = FALSE,
                       too_few = "align_start") |> 
  mutate(new_height = case_when(
    !is.na(new_height) & new_height >= 50 & new_height <= 84 ~ new_height,
    str_detect(height, cm_pattern) ~ new_height / 2.54,
    str_detect(height, m_pattern) ~ as.numeric(str_replace(height, ",", ".")) * 100 / 2.54,
    str_detect(height, ft_pattern) ~ as.numeric(str_remove(height, "'")) * 12,
    !is.na(feet) & !is.na(inches) ~ as.numeric(feet) * 12 + as.numeric(inches)
  ),
   new_height = ifelse(new_height < 50 | new_height > 84, NA, new_height)) |> 
  select(-c(feet, inches))

fixed_heights |> 
  filter(is.na(new_height)) |> 
  pull(height)

