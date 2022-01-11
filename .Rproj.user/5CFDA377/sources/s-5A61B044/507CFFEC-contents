
# CHAFFINCH

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
## importing the data on chaffinch (Fringilla coelebs)
Chaffinch_2018 <- read_delim("Data/Chaffinch_2018/0067140-210914110416597.csv",
                             delim = "\t", escape_double = FALSE,
                             trim_ws = TRUE)

# DATA TIDYING -----------------------------------------------------------------
Chaffinch_2018 <- Chaffinch_2018 %>%
  dplyr::select(species, decimalLongitude, decimalLatitude) %>% # in this case you can notice that we wrote down dplyr::select instead of just typing command select(). This is because the command select() is in two our libraries. Therefore, we wanted to specify that this select is from dplyr package which is within tidyverse.
  rename(latitude = decimalLatitude,
         longitude = decimalLongitude)

## writing a csv file Waterlouse_2018.csv into the folder Waterlouse_2018
write.csv(Chaffinch_2018, "Data/Chaffinch_2018/Chaffinch_2018.csv")

## libraries needed: library(rgdal)
## importing data on chaffinch as a shapefile from the folder
## Chaffinch_2018_shp in the folder Chaffinch_2018
Chaffinch_2018 <- readOGR("Data/Chaffinch_2018/Chaffinch_2018_shp", "Chaffinch_2018")
plot(Chaffinch_2018)

## adjusting coordinate systems
## libraries needed: library(sp)
proj4string(Nitrogen_2018) <- CRS("+init=epsg:28992")

Chaffinch_2018 <- spTransform(Chaffinch_2018, proj4string(Nitrogen_2018))

# checking whether both files are in the same coordinate system
proj4string(Chaffinch_2018)
proj4string(Nitrogen_2018)

# PLOTTING ---------------------------------------------------------------------
plot(Nitrogen_2018)
points(Chaffinch_2018)

## merging datasets
## library needed: library(raster)
Chaffinch_2018$Nitrogen_2018 <- extract(Nitrogen_2018, Chaffinch_2018)

## writing a data table with waterlouse occurrences and Nitrogen deposition levels
data <- as.data.frame(Chaffinch_2018) %>%
  na.omit()

# BOXPLOT
boxplot(Nitrogen_2018 ~ species,
        data = data,
        main = "Average Nitrogen deposition per species occurrence",
        xlab = "Chaffinch (Fringilla coelebs)",
        ylab = "Nitrogen deposition (mol/(ha.year))",
        col = "orange",
        border = "brown")

# BARCHART
ggplot(data) +
  geom_bar(aes(Nitrogen_2018)) +
  labs(x = "Nitrogen deposition (mol/(ha.year))",
       y = "Chaffinch (Fringilla coelebs) (n)")

# FREQUENCY POLYGON
f <- ggplot(data) +
  geom_freqpoly(aes(Nitrogen_2018)) +
  labs(title = "Species occurences per Nitrogen deposition",
       x = "Nitrogen deposition (mol/(ha.year))",
       y = "Chaffinch (Fringilla coelebs) (n)")


# ANALYSIS ---------------------------------------------------------------------
## writing a function which extracts values from frequency polygon
get_fpoly <- function(p) {
  d <- ggplot_build(p)$data[[1]]
  data.frame(x = d$x, xmin = d$xmin, xmax = d$xmax, y = d$y)
}

x <- get_fpoly(f)$x
y <- get_fpoly(f)$y

## Calculating correlation coefficient using cor.text() method
result <-  cor.test(x, y, method = "pearson")

## printing the result
print(result)


