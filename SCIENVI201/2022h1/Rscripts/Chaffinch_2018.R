
# CHAFFINCH

## Before starting with the assignment, you always need to install the libraries you have not used before with the code install.packages(...).
## Once you have installed them you do not need to do it again, but you need to load them at the beginning of every session with the code library(...).

install.packages("tidyverse")
install.packages("tabularaster")
install.packages("terra")
install.packages("sf")
install.packages("readr")
install.packages("tidymodels")

# LIBRARIES --------------------------------------------------------------------
## The list of all the libraries that you need for completing the assignment.
## Every section has a comment which says which library you are using.

library(tidyverse)
library(tabularaster)
library(terra)
library(sf)
library(readr)
library(tidymodels)

## Once you have all the libraries loaded, we will set a working directory, a root of the project in your PC, so we can find all the files related to our project at one place.
## This can be done also by clicking Session (toolbar in the top), working directory, choose directory, and navigating to the folder that contains your data.
setwd("C://Users/PC/Documents/GitHub/projects") # update to your own working directory


# DATA IMPORT ------------------------------------------------------------------
## Nitrogen data
## libraries needed: library(sf) and library(terra)
## Go to the website: https://www.rivm.nl/gcn-gdn-kaarten/depositiekaarten/cijfers-achter-depositiekaarten/gdn-depositiebestanden-achterliggende-jaren, and download total Nitrogen deposition (Total stikstof (N)) from the year 2018 (ndep 2018).
## Once downloaded, you can see that the data is in a zipped file which cannot be worked with in R.
## Therefore, now you extract all with right click, and save the extracted data into a new folder called Data in your Ecology Project file.
## I renamed the folder from depo_ntot_2018 to nitrogen for a simple overview, but you can order your files according to your preferences.
## If something went wrong, the Nitrogen data can be found on github: projects/Environmental science/Nitrogen deposition effect on biodiveristy/Data/nitrogen
## importing the data on Nitrogen deposition levels
Nitrogen_2018 <- rast("SCIENVI201/2022h1/Data/nitrogen/depo_ntot_2018.asc")

## Lets see how does Nitrogen deposition map looks like.
Nitrogen_2018 %>%
  as.data.frame(xy=T) %>%
  as_tibble() %>%
  ggplot(aes(x = x, y = y, fill = depo_ntot_2018)) +
  geom_raster()

## Species data
## libraries needed: library(readr)
## Go to the website: gbif.org, click get data (toolbar on the top) and select occurrences.
## Now select, the occurrences of your species in your preferred region.
## Go to the toolbar on the left, under Scientific name type: Fringilla coelebs, in Year select: 2018, and finally in Country or area select: Netherlands.
## Click DOWNLOAD, create an account, and select SIMPLE in download options. This step might take a little while, as the request needs to be processed.
## In the meantime create a chaffinch folder in your Data folder where you will store all the information on your species.
## Once the file is ready, click download, and save the data. It is again a zipped file which needs to be extracted.
## With the right click click extract all and save the data into chaffinch folder in the Data folder.
## As a last step, rename the extracted csv file to chaffinch_2018.
## Again, if something went wrong, the Nitrogen data can be found on github: projects/Environmental science/Nitrogen deposition effect on biodiveristy/Data/chaffinch/chaffinch_2018.csv
## importing the data on chaffinch (Fringilla coelebs)
Chaffinch_2018 <- read_delim("SCIENVI201/2022h1/Data/chaffinch/chaffinch_2018.csv",
                             delim = "\t", escape_double = FALSE, trim_ws = TRUE)

## Lets see where does the chaffinch occur on the map
ggplot(Chaffinch_2018) +
  geom_point(aes(decimalLongitude, decimalLatitude))

## Lets see how this data set looks like.
Chaffinch_2018 %>% view()

## It is a quiet big data set with 50 columns and over 60000 observations (rows). However, we do not need all the information.
## Therefore, in the next step we will clean our data set to our preferred size.

# DATA TIDYING -----------------------------------------------------------------
## In this following code we select the variables with which we will be working: species names (species), longitude (decimalLongitude), and latitude (decimalLatitude)
Chaffinch_2018 <- Chaffinch_2018 %>%
  dplyr::select(species, decimalLongitude, decimalLatitude) %>% # in this case you can notice that we wrote down dplyr::select instead of just typing command select(). This is because the command select() is in two of our libraries. Therefore, we wanted to specify that this select is from dplyr package which is within tidyverse.
  na.omit() %>%
  st_as_sf(coords = c("decimalLongitude","decimalLatitude"))

## libraries needed: library(sf)
## When working with geospatial data, we can come across various versions of coordinate systems. Therefore, our next step is to check whether our two geospatial data (Nitrogen and Chaffinch) is in the same coordinate system.
## The code crs(data) tells us in which coordinate system the geospatial data is. Here, the Nitrogen deposition file does not have it defined, but reading the information on the dataset we know that the dataset is in Amersfoort coordinate system.
## Therefore, we can tell the Nitrogen_2018 that it is in this format with the following code:
crs(Nitrogen_2018) <- "+init=epsg:28992"

## Afterwards we can adjust the chaffinch coordinate system into the same coordinate system.
st_crs(Chaffinch_2018) <- "+init=epsg:4326"
Chaffinch_2018 <- st_transform(Chaffinch_2018, 28992)

## library needed: library(terra)
## Now, we do not only want to be able to see our data on a graph but also in a numeric dataset. In another words, we want to see which geospatial point belongs to which Nitrogen level and the number of species occuring.
## Therefore, here we want to merge these two files and write it into a table complete_data
complete_data <- terra::extract(Nitrogen_2018, vect(Chaffinch_2018), cells = T, xy = T) %>% as_tibble()

complete_data <- complete_data %>%
  mutate(species = "Fringilla coelebs")

# PLOTTING ---------------------------------------------------------------------
## Once, we have our datasets available we would like to look at a graph which would show us whether there is some relationship between the level of Nitrogen and species occurrences.
## In the following lines of code, we have three various graphs and you need to justify yourself which one is the most informative.

# BOXPLOT
boxplot(depo_ntot_2018 ~ species,
        data = complete_data,
        main = "Average Nitrogen deposition per species occurrence",
        xlab = "Chaffinch, Fringilla coelebs",
        ylab = "Nitrogen deposition (mol/(ha.year))",
        col = "orange",
        border = "brown")

# BARCHART
ggplot(complete_data) +
  geom_bar(aes(depo_ntot_2018)) +
  labs(x = "Nitrogen deposition (mol/(ha.year))",
       y = "Chaffinch, Fringilla coelebs (n)")

# FREQUENCY POLYGON
ggplot(complete_data) +
  geom_freqpoly(aes(depo_ntot_2018)) +
  labs(title = "Species occurences per Nitrogen deposition",
       x = "Nitrogen deposition (mol/(ha.year))",
       y = "Chaffinch (Fringilla coelebs) (n)")

# ANALYSIS ---------------------------------------------------------------------
## After visually observing the relationship, now we want to see the quantitative analysis.
## calling our frequency polygon f
f <- ggplot(complete_data) +
  geom_freqpoly(aes(depo_ntot_2018)) +
  labs(title = "Species occurences per Nitrogen deposition",
       x = "Nitrogen deposition (mol/(ha.year))",
       y = "Chaffinch (Fringilla coelebs) (n)")

## writing a function which extracts values from frequency polygon
get_fpoly <- function(p) {
  d <- ggplot_build(p)$data[[1]]
  data.frame(x = d$x, xmin = d$xmin, xmax = d$xmax, y = d$y)
}

## extracting x and y variables with the extracting function (get_fpoly) from frequency polygen (f).
x <- get_fpoly(f)$x
y <- get_fpoly(f)$y

## writing a table with x and y variables and calling it t
t <- tibble(x, y)

## visualizing the relationship
ggplot(t, aes(x, y)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ poly(x, 2), se = FALSE)

## library needed: library(tidymodels)
## statistical resutls
lm(y~poly(x, 2),t) %>%
  tidy()

