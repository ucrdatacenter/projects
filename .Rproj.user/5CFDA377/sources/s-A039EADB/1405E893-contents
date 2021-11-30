
# libraries --------------------------------------------------------------------
library(tidyverse)
library(sp)
library(raster)
library(rgdal)
library(readr)

# Nitrogen data ----------------------------------------------------------------
Nitrogen_2018 <- raster("RIVM_Nitrogen/depo_ntot_2018.asc")
plot(Nitrogen_2018)

# Asellus data -----------------------------------------------------------------
Asellus_2018 <- read_delim("Asellus_2018/0067140-210914110416597.csv",
                           delim = "\t", escape_double = FALSE,
                           trim_ws = TRUE)
Asellus_2018 <- Asellus_2018 %>%
  dplyr::select(species, decimalLongitude, decimalLatitude)

write.csv(Asellus_2018, "Asellus_2018/Asellus_2018.csv")

Asellus_2018 <- readOGR("Asellus_2018/Asellus_2018_shp", "Asellus")
plot(Asellus_2018)

# Plotting nitrogen and asellus ------------------------------------------------
plot(Nitrogen_2018)
points(Asellus_2018)

# merging datasets -------------------------------------------------------------
Asellus_2018$Nitrogen_2018 <- extract(Nitrogen_2018, Asellus_2018)

Natura2000 <- readOGR("Natura2000", "Natura2000_end2018_epsg3035")
Natura2000 <- spTransform(Natura2000, proj4string(Nitrogen_2018))

Nmasked <- mask(Nitrogen_2018, Natura2000)
plot(Nmasked)
plot(Natura2000, add=TRUE)

Asellus_2018$Natura2000 <- extract(Nmasked, Asellus_2018)
Asellus_2018$Natura2000 <- ifelse(is.na(Asellus_2018$Natura2000), 0, 1)

dat <- as.data.frame(Asellus_2018)
table(complete.cases(dat))
dat <- na.omit(dat)
head(dat)

write.table(dat, file="new_asellus_data", append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE)

boxplot(Nitrogen_2018~ species,
        data = dat,
        main = "Average Nitrogen deposition per species occurrence",
        xlab = "Species",
        ylab = "Nitrogen deposition",
        col = "orange",
        border = "brown")

boxplot(Nitrogen_2018 ~ Natura2000,
        data = dat,
        main = "Average Nitrogen deposition in Natura 2000 sites vs in other sites",
        xlab = "Natura 2000",
        ylab = "Nitrogen deposition",
        col = "orange",
        border = "brown")

N2000Y <- subset(dat, Natura2000 == 1)
N2000N <- subset(dat, Natura2000 == 0)
t.test(N2000Y$Nitrogen, N2000N$Nitrogen)
