
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

Nitrite %>%
  ggplot()+
  geom_point(aes(Year, Mean, color = WaterbaseID))


# identifying rivers -----------------------------------------------------------

WaterStations <- read_csv("Waterbase Rivers/Waterbase_rivers_v14_Stations.csv")

NitriteRivers <- left_join(Nitrite, WaterStations, by = "WaterbaseID") %>%
  select(RiverName, Year, Mean, Unit_Nutrients) %>%
  rename(river = RiverName,
         year = Year,
         mean = Mean,
         unit = Unit_Nutrients)

write.csv(NitriteRivers, "NitriteRivers.csv")
