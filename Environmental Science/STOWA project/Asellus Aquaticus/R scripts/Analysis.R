
# libraries --------------------------------------------------------------------
library(tidyverse)
library(readr)

# merging Asellus Aquaticus and Nitrite datasets -------------------------------

# importing a dataset of rivers with gps coordinates from NaturalEarth
rivers_NE <- read_csv("PolygonConverted.csv")

rivers_NE <- rivers_NE %>%
  select(featurecla, name, name_nl, Longitude, Latitude) %>%
  rename(river_name = name_nl)


NitriteRivers <- NitriteRivers %>%
  mutate(river_name = tolower(river)) %>%
  select(river_name, year, mean, unit)

capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

NitriteRivers$river_name <- capFirst(NitriteRivers$river_name)

left_join(NitriteRivers, rivers_NE) %>% view()

# ------------------------------------------------------------------------------
