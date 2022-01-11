
# WATERLOUSE

## Before starting with the assignment, do not forget that you need to install
## the libraries you have not used before with the code install.packages(...)

# LIBRARIES --------------------------------------------------------------------
## the list of all the libraries that you need for completing the assignment
## every section has a comment which says which library you are using
library(tidyverse)
library(sp)
library(raster)
library(rgdal)
library(readr)

# DATA IMPORT ------------------------------------------------------------------
## libraries needed: library(sp) and library(raster)
## importing the data on Nitrogen deposition levels
Nitrogen_2018 <- raster("Data/Nitrogen_2018/depo_ntot_2018.asc")
## lets see how does Nitrogen deposition map looks like
plot(Nitrogen_2018)

## libraries needed: library(readr)
## importing the data on waterlouse (Asellus Aquaticus)
Waterlouse_2018 <- read_delim("Data/Waterlouse_2018/0067140-210914110416597.csv",
                              delim = "\t", escape_double = FALSE,
                              trim_ws = TRUE)
Waterlouse_2018 <- Waterlouse_2018 %>%
  dplyr::select(species, decimalLongitude, decimalLatitude) %>% # in this case you can notice that we wrote down dplyr::select instead of just typing command select(). This is because the command select() is in two our libraries. Therefore, we wanted to specify that this select is from dplyr package which is within tidyverse.
  rename(latitude = decimalLatitude,
         longitude = decimalLongitude)

## writing a csv file Waterlouse_2018.csv into the folder Waterlouse_2018
write.csv(Waterlouse_2018, "Data/Waterlouse_2018/Waterlouse_2018.csv")

## libraries needed: library(rgdal)
## importing data on Waterlouse as a shapefile from the folder
## Waterlouse_2018_shp in the folder Waterlouse_2018
Waterlouse_2018 <- readOGR("Data/Waterlouse_2018/Waterlouse_2018_shp", "Waterlouse_2018")
plot(Waterlouse_2018)

# ADJUSTING COORDINATE SYSTEMS -------------------------------------------------
## libraries needed: library(sp)
proj4string(Nitrogen_2018) <- CRS("+init=epsg:28992")
Waterlouse_2018 <- spTransform(Waterlouse_2018, proj4string(Nitrogen_2018))

## checking whether both files are in the same coordinate system
proj4string(Waterlouse_2018)
proj4string(Nitrogen_2018)

# PLOTTING ---------------------------------------------------------------------
plot(Nitrogen_2018)
points(Waterlouse_2018)

# MERGING DATASETS -------------------------------------------------------------
## library needed: library(raster)
Waterlouse_2018$Nitrogen_2018 <- extract(Nitrogen_2018, Waterlouse_2018)

## writing a data table with waterlouse occurrences and Nitrogen deposition levels
data <- as.data.frame(Waterlouse_2018) %>%
  na.omit()

# BOXPLOT ----------------------------------------------------------------------
boxplot(Nitrogen_2018 ~ species,
        data = data,
        main = "Average Nitrogen deposition per species occurrence",
        xlab = "Waterlouse (Asellus Aquaticus)",
        ylab = "Nitrogen deposition (mol/(ha.year))",
        col = "orange",
        border = "brown")

# BARCHART ---------------------------------------------------------------------
ggplot(data) +
  geom_bar(aes(Nitrogen_2018)) +
  labs(x = "Nitrogen deposition",
       y = "Waterlouse (Asellus Aquaticus)")

# FREQUENCY POLYGON ------------------------------------------------------------
f <- ggplot(data) +
  geom_freqpoly(aes(Nitrogen_2018)) +
  labs(title = "Species occurences per Nitrogen deposition",
       x = "Nitrogen deposition (mol/(ha.year))",
       y = "Waterlouse (Asellus Aquaticus) (n)")

# CORRELATION TEST -------------------------------------------------------------
## writing a function which extracts values from frequency polygon
get_fpoly <- function(p) {
  d <- ggplot_build(p)$data[[1]]
  data.frame(x = d$x, xmin = d$xmin, xmax = d$xmax, y = d$y)
}

x <- get_fpoly(f)$x
y <- get_fpoly(f)$y

## Calculating correlation coefficient using cor.text() method
result <-  cor.test(x, y, method = "spearman")

## printing the result
print(result)

