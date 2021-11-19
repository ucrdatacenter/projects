
# libraries -------------------------------------------------------------------
library(tidyverse)
library(readr)

# data -------------------------------------------------------------------------

WaterNutrients <- read_csv("Waterbase Rivers/Waterbase_rivers_v14_Nutrients.csv")

Nitrite <- WaterNutrients %>%
  filter(CountryCode == "NL") %>%
  filter(Determinand_Nutrients == "Nitrite") %>%
  select(WaterbaseID, NationalStationID, Year, Unit_Nutrients, Mean)

Nitrite <- Nitrite %>%
  filter(Year >= 2000 & Year <= 2010)

write.csv(Nitrite, "Nitrite.csv")

# graphing ---------------------------------------------------------------------

Nitrogen %>%
  ggplot()+
  geom_point(aes(Year, Mean))
