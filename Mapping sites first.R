#Mapping the gps sites first, then adding the Maldives (Ants course)
#Hannah 
## 14 Dec
##22 Dec 2024

# library(readr)
# library(tidyr) 
# library(ggplot2)
# library(dplyr)
# library(lubridate)
library(tidyverse)
library(sf)
# library(terra)
# install.packages("abind")
# install.packages("statmod")
# install.packages("leaflet.extras")
# library(stars)
# library(statmod)
# library(tmap) 
# library(mgcv)
# install.packages("st_read")
library(rnaturalearth)
# library(abind)
library(readxl)
library(ggrepel)


# Load your data points
dat <- read_excel("Data/Maldives_TO_2021_survey_2024_10_10.xlsx", sheet = "Sites_R", na = "NA")

ggplot(dat) + 
  aes(x = Longitude, y = Latitude, color = Frequency) +
  geom_point()

#Intro to maps
library(sf)
sdat <- st_as_sf(dat, coords = c("Longitude", "Latitude"), 
                 crs = 32643)
crs32643 <- st_crs(32643)
crs32643$Name # name of the crs

crs32643$proj4string # crs as a proj4string
crs32643$wkt # crs in well-known text format


#Plotting sites & frequency of site use
sdat
plot(sdat)
plot(sdat["Frequency"])

tm_shape(sdat) + 
  tm_dots(col = "Frequency")

tm1 <- tm_shape(sdat) + 
  tm_dots(col = "Frequency", 
          palette = "Blues", 
          title = "MRW Sites")
tm1

tmap_save(tm1, filename = "Plots/Site_map.png", 
          width = 600, height = 600)

##Mapping Maldives
Maldives <- ne_countries(scale = "medium", country = "Maldives", returnclass = "sf")


#Loading shape files
#Not happy with how this worked, trying new shape files below ".._Joanna"
#atolls <- st_read("Data/mdv_adm_gov_20241022_ab_shp/mdv_admbnda_adm1_gov_20241022.shp")  ####THE OTHER SHAPE FILE

atolls <- st_read("Data/Maldives_atoll_shapefile_Joanna/Maldives_v8.shp")

# Assuming your 'dat' dataframe has 'Latitude' and 'Longitude' columns
sites_sf <- st_as_sf(dat, coords = c("Longitude", "Latitude"), crs = 4326)

sites_centroids <- st_centroid(sites_sf)

m1 <- ggplot() +
  geom_sf(data = atolls, fill = "lightblue", color = "black") +
  geom_sf(data = Maldives, fill = "lightblue", color = "black") +
  geom_sf(data = sites_sf, color = "red", size = 2) +
  # geom_sf_text(data = sites_sf, aes(label = Site_name), nudge_x = 0.1, size = 2) +
  geom_text_repel(data = sites_centroids, aes(
    x = st_coordinates(sites_centroids)[, 1], 
    y = st_coordinates(sites_centroids)[, 2], 
    label = Site_name), 
    size = 2, 
    # min.segment.length = 0, 
    box.padding = 0.2, 
    point.padding = 0.2) +
  coord_sf(xlim = c(72, 74.5), ylim = c(-0.5, 7.5)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_x_continuous(breaks = seq(72, 74, by = 1)) +
  xlab("") +
  ylab("")

ggsave("Plots/Site_map_atolls.pdf", plot = m1, width = 6, height = 6, units = "in", dpi = 300)
  
