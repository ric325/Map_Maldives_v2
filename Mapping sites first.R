#Mapping the gps sites first, then adding the Maldives (Ants course)
#Hannah 
## 14 Dec
##22 Dec 2024

library(readr)
library(tidyr) 
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)
library(sf)
library(terra)
install.packages("abind")
install.packages("statmod")
library(stars)
library(statmod)
library(tmap) 
library(mgcv)
install.packages("st_read")
library(rnaturalearth)
library(abind)
library(readxl)


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

tmap_save(tm1, filename = "Site_map.png", 
          width = 600, height = 600)

##Mapping Maldives
Maldives <- ne_countries(scale = "medium", country = "Maldives", returnclass = "sf")
Maldives

#Loading shape files
#Not happy with how this worked, trying new shape files below ".._Joanna"
#atolls <- st_read("Data/mdv_adm_gov_20241022_ab_shp/mdv_admbnda_adm1_gov_20241022.shp")  ####THE OTHER SHAPE FILE

atolls <- st_read("Data/Maldives_atoll_shapefile_Joanna/Maldives_v8.shp")

atolls

tm_shape(atolls) + 
  tm_polygons()

tm_shape(atolls) + 
  tm_polygons(col = 'lightblue') +
  tm_shape(Maldives) + 
  tm_polygons() + 
  tm_shape(sdat) + 
  tm_dots()

tmap_options(check.and.fix = TRUE)

# Assuming your 'dat' dataframe has 'Latitude' and 'Longitude' columns
sites_sf <- st_as_sf(dat, coords = c("Longitude", "Latitude"), crs = 4326)

tm_shape(atolls) + 
  tm_polygons(col = 'lightblue') +
  tm_shape(Maldives) + 
  tm_polygons() + 
  tm_shape(sites_sf) + 
  tm_dots()


st_crs(atolls)
st_crs(Maldives)

##this map below works, it's blue atolls, some admin boundaries, and sites
#now I want to add site names

# Assuming you have a column named 'Site_name' 
tm_shape(atolls) + 
  tm_polygons(col = 'lightblue') +
  tm_shape(Maldives) + 
  tm_polygons() + 
  tm_shape(sites_sf) + 
  tm_dots() +
  tm_text("Site_name", size = 0.8, along.lines = FALSE, ymod = -0.30, xmod = 0.2)

###still overlapping


library(ggplot2)

ggplot() +
  geom_sf(data = atolls, fill = "lightblue", color = "black") +
  geom_sf(data = Maldives, fill = "lightblue", color = "black") +
  geom_sf(data = sites_sf, color = "red", size = 2) +
  geom_sf_label(data = sites_sf, aes(label = Site_name), nudge_x = 0.1) +
  theme_bw()

m1 <- ggplot() +
  geom_sf(data = atolls, fill = "lightblue", color = "black") +
  geom_sf(data = Maldives, fill = "lightblue", color = "black") +
  geom_sf(data = sites_sf, color = "red", size = 2) +
  geom_sf_label(data = sites_sf, aes(label = Site_name), nudge_x = 0.1) +
  coord_sf(xlim = c(72.5, 73.8), ylim = c(-0.5, 7.5)) +  # Adjust the limits as needed
  theme_bw()



ggsave("Site_map_atolls.png", plot = m1, width = 6, height = 6, units = "in", dpi = 300)














#using other packages

install.packages(c("leaflet", "plotly")) 

library(leaflet)

library(tidyverse)

# Assuming the concatenated column is named 'Longitude_Latitude'
#sites_sf <- sites_sf %>%
  separate(Degrees_Minutes_Seconds, into = c("Longitude", "Latitude"), sep = ",") %>%
  mutate(Longitude = as.numeric(Longitude), Latitude = as.numeric(Latitude))
  
  
  # Extract coordinates
  coords <- st_coordinates(sites_sf)
  
  # Add coordinates as new columns to the data frame
  sites_sf$Longitude <- coords[, 1]
  sites_sf$Latitude <- coords[, 2]
  
  # Assuming the format is "Longitude, Latitude"
  sites_sf$Longitude <- as.numeric(str_split_fixed(sites_sf$Longitude_Latitude, ",", 2)[, 1])
  sites_sf$Latitude <- as.numeric(str_split_fixed(sites_sf$Longitude_Latitude, ",", 2)[, 2])


  
  leaflet() %>%
    addTiles() %>%
    addPolygons(data = atolls, weight = 1, color = "black", fillColor = "lightblue", fillOpacity = 0.5) %>%
    addPolygons(data = Maldives, weight = 1, color = "black", fillColor = "lightblue", fillOpacity = 0.5) %>%
    addCircleMarkers(data = sites_sf, lat = ~Latitude, lng = ~Longitude, popup = ~Site_name)
  

##great it worked! Admin atolls and site points (no names) and Dhevihi writing on the top of it
  #now to fix the points so they are smaller
  
  leaflet() %>%
    addTiles() %>%
    addPolygons(data = atolls, weight = 1, color = "black", fillColor = "lightblue", fillOpacity = 0.5) %>%
    addPolygons(data = Maldives, weight = 1, color = "black", fillColor = "darkblue", fillOpacity = 0.5) %>%
    addCircleMarkers(data = sites_sf, lat = ~Latitude, lng = ~Longitude, 
                     popup = ~paste0("<b>Site Name:</b> ", Site_name))


  leaflet() %>%
    addTiles() %>%
    addPolygons(data = atolls, weight = 1, color = "black", fillColor = "lightblue", fillOpacity = 0.5) %>%
    addPolygons(data = Maldives, weight = 1, color = "black", fillColor = "darkblue", fillOpacity = 0.5) %>%
    addCircleMarkers(data = sites_sf, lat = ~Latitude, lng = ~Longitude, 
                     popup = ~paste0("<b>Site Name:</b> ", Site_name),
                     radius = 1,  # Adjust the radius as needed
                     color = "red",  # Set the marker color
                    fillOpacity = 1)

  
  ##Great!! Now to add labels
  library(leaflet)
  
  leaflet() %>%
    addTiles() %>%
    addPolygons(data = atolls, weight = 1, color = "black", fillColor = "lightblue", fillOpacity = 0.5) %>%
    addPolygons(data = Maldives, weight = 1, color = "black", fillColor = "darkblue", fillOpacity = 0.5) %>%
    addCircleMarkers(data = sites_sf, lat = ~Latitude, lng = ~Longitude, 
                     popup = ~paste0("<b>Site Name:</b> ", Site_name),
                     radius = 1, 
                     color = "red",
                     fillOpacity = 1) %>%
    addLabelOnlyMarkers(data = sites_sf, 
                        lat = ~Latitude, 
                        lng = ~Longitude,
                        label = ~Site_name,
                        labelOptions = labelOptions(noHide = TRUE, direction = "side", offset = c(0, 10)))
  
  ##Lines that draw the labels to the points
  library(leaflet)
  install.packages("leaflet.extras")
  library(leaflet.extras)
  
  leaflet() %>%
    addTiles() %>%
    addPolygons(data = atolls, weight = 1, color = "black", fillColor = "lightblue", fillOpacity = 0.5) %>%
    addPolygons(data = Maldives, weight = 1, color = "black", fillColor = "darkblue", fillOpacity = 0.5) %>%
    addCircleMarkers(data = sites_sf, lat = ~Latitude, lng = ~Longitude, 
                     popup = ~paste0("<b>Site Name:</b> ", Site_name),
                     radius = 1, 
                     color = "red",
                     fillOpacity = 1) %>%
    addLabelOnlyMarkers(data = sites_sf, 
                        lat = ~Latitude, 
                        lng = ~Longitude,
                        label = ~Site_name,
                        labelOptions = labelOptions(noHide = TRUE, direction = "side", offset = c(0, 10)),
                        clusterOptions = markerClusterOptions())

  
  
  tmap_save(tm1, filename = "Site_map_atolls.png", 
            width = 600, height = 600)
  
  
##FAILED Its only in viewer!!!
  install.packages("plotly")
  library(plotly)
  
  fig <- plot_geo() %>%
    add_trace(
      data = atolls,
      geojson = jsonlite::toJSON(st_geometry(atolls)),
      fillcolor = 'lightblue',
      line = list(color = 'black', width = 1)
    ) %>%
    add_trace(
      data = Maldives,
      geojson = jsonlite::toJSON(st_geometry(Maldives)),
      fillcolor = 'lightblue',
      line = list(color = 'black', width = 1)
    ) %>%
    add_markers(
      data = sites_sf,
      lat = ~Latitude,
      lon = ~Longitude,
      text = ~Site_Name,
      hoverinfo = "text"
    )
  
  fig
  
  
  fig <- plot_geo() %>%
    add_trace(
      data = atolls,
      geojson = jsonlite::toJSON(st_geometry(atolls)),
      fillcolor = 'lightblue',
      line = list(color = 'black', width = 1)
    ) %>%
    add_trace(
      data = Maldives,
      geojson = jsonlite::toJSON(st_geometry(Maldives)),
      fillcolor = 'lightblue',
      line = list(color = 'black', width = 1)
    ) %>%
    add_markers(
      x = ~Longitude,
      y = ~Latitude,
      data = sites_sf,
      text = ~Site_name,
      hoverinfo = "text"
    )
  fig
  
  

#########


tm_shape(atolls, bbox = sdat) + 
  tm_polygons()+#col = 'lightblue') +
  tm_shape(Maldives) + 
  tm_polygons() + 
  tm_shape(sdat) + 
  tm_dots()



# Load the atolls data
atolls_sf <- st_read("Data/mdv_admbnda", layer = "mdv_admbnda_adm1")

getwd()
file.exists("data/maldives_admin_atoll")

