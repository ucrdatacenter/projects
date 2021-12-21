
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

# importing Asellus as a shapefile ---------------------------------------------
Asellus_2018 <- readOGR("Asellus_2018/Asellus_2018_shp", "Asellus")
plot(Asellus_2018)

# Plotting nitrogen and asellus ------------------------------------------------
plot(Nitrogen_2018)
points(Asellus_2018)

# merging datasets -------------------------------------------------------------
Asellus_2018$Nitrogen_2018 <- extract(Nitrogen_2018, Asellus_2018)

dat <- as.data.frame(Asellus_2018)
table(complete.cases(dat))
dat <- na.omit(dat)

# exporting asellus data -------------------------------------------------------
write.table(dat, file = "new_asellus_data", append = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)

# potting a boxplot with average Nitrogen deposition per species occurrence
boxplot(Nitrogen_2018~ species,
        data = dat,
        main = "Average Nitrogen deposition per species occurrence",
        xlab = "Species",
        ylab = "Nitrogen deposition",
        col = "orange",
        border = "brown")

# plotting barchart
ggplot(dat) +
  geom_bar(aes(Nitrogen_2018)) +
  labs(x = "Nitrogen deposition",
       y = "Species")

ggplot(dat) +
  geom_freqpoly(aes(Nitrogen_2018)) +
  labs(title = "Species occurences per Nitrogen deposition",
       x = "Nitrogen deposition",
       y = "Species")

# performing a t test ----------------------------------------------------------
Nhigh <- subset(dat, Nitrogen_2018 > 1000)
Nlow <- subset(dat, Nitrogen_2018 < 1000)
t.test(Nhigh$Nitrogen_2018, Nlow$Nitrogen_2018)
