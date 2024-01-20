library(tidyverse)
library(tidygeocoder)
library(sf)
library(leaflet)
library(leafem)
library(rnaturalearth)
library(rnaturalearthhires)
library(rnaturalearthdata)
library(terra)
library(tidyterra)

latlong <- tribble(
  ~longitude, ~latitude, ~char,
  5,          51,        1,
  6,          52,        2
) 

latlong |> 
  leaflet() |> 
  addTiles() |> 
  addCircleMarkers(lng = ~longitude, lat = ~latitude) |> 
  addMouseCoordinates()

latlong |> 
  st_as_sf(coords = c("longitude","latitude"), crs = "WGS84") |> 
  ggplot() +
  geom_sf(aes(color = as_factor(char)))


pg <- tribble(
  ~longitude,  ~latitude,
  10,   0,
  11,   0,
  12,   4,
  10,   0
) |> 
  as.matrix() |> 
  list() |> 
  st_polygon()
pt <- c(2, 2) |> st_point()
g <- st_sfc(pg, pt)
st_sf(tibble(char = 8:9), geometry = g) |> 
  ggplot() +
  geom_sf(aes(color = as_factor(char)))

latlong <- tribble(
  ~name,                  ~addr,
  "UCR",          "Lange Noordstraat 1, Middelburg, Netherlands"
) |> 
  geocode(addr, method = 'osm', lat = latitude , long = longitude)

neth <- ne_countries(scale = 10, returnclass = "sf", country = "netherlands") 
latlong |> 
  st_as_sf(coords = c("longitude","latitude"), crs = "WGS84") |>
  ggplot() +
  geom_sf(data = neth) +
  geom_sf_text(aes(label = name)) +
  coord_sf(xlim = c(3, 8), ylim = c(50.5, 54))


Nitrogen_2018 <- 
  rast("https://raw.githubusercontent.com/ucrdatacenter/projects/main/SCIENVI201/2022h1/Data/nitrogen/depo_ntot_2018.asc")
crs(Nitrogen_2018) <- "epsg:28992"
ggplot() +
  geom_spatraster(data = Nitrogen_2018)

Chaffinch_2018 <- 
  read_delim("https://raw.githubusercontent.com/ucrdatacenter/projects/main/SCIENVI201/2022h1/Data/chaffinch/chaffinch_2018.csv",
             delim = "\t", escape_double = FALSE, trim_ws = TRUE) 

birds <- Chaffinch_2018 |> 
  select(species, decimalLongitude, decimalLatitude) |> 
  drop_na() |> 
  count(decimalLongitude, decimalLatitude) |> 
  st_as_sf(coords = c("decimalLongitude","decimalLatitude"), crs = "WGS84")
birds <- st_transform(birds, "epsg:28992")
  
birds |> 
  ggplot() +
  geom_spatraster(data = Nitrogen_2018) +
  geom_sf(aes(size = n))

done <- terra::extract(Nitrogen_2018, birds, xy = T, bind = T) |> as_tibble()
done |> 
  ggplot(aes(depo_ntot_2018, n)) + geom_point() + geom_smooth()
  
