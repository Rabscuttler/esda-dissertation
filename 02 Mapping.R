library(tidyverse)
library(dplyr)
library(lubridate)
library(rgdal)
library(raster)
library(tmap)
library(tmaptools)
library(sf)
library(forcats)
tmap_mode("plot")

# Create overlay map
base_dir = "data/gb-outlines_2856975/"
england <- readOGR(paste(base_dir, "england.shp", sep='')) %>% st_as_sf("sf")
wales <- readOGR(paste(base_dir, "wales.shp", sep='')) %>% st_as_sf("sf")
scotland <- readOGR(paste(base_dir, "scotland.shp", sep='')) %>% st_as_sf("sf")
greatbritain <- readOGR(paste(base_dir, "greatbritain.shp", sep='')) %>% st_as_sf("sf")

gb <- 
  # Read GB outline, and set bounding box to cut off shetlands
  tm_shape(greatbritain, bbox = bb(greatbritain, ylim=c(.0, .76), relative = TRUE)) + 
  tm_borders(col="black") +
  tm_shape(england) +
  tm_fill(col="grey",alpha = 0.1) +
  tm_shape(wales) +
  tm_fill(col="grey",alpha = 0.4) +
  tm_shape(scotland) +
  tm_fill(col="grey",alpha = 0.4) +
  tm_scale_bar(position = c("right","bottom")) + 
  tm_layout(legend.position=c("RIGHT","TOP"),inner.margins = c(0.04, .02, .05, .06),legend.title.size = 1)+
  tm_layout(frame=F)

gb

# Regions
regions <- readOGR("data/district_borough_unitary.shp") %>% st_as_sf("sf")
tm_shape(regions) + tm_borders(col="black")

# Postcodes ---------------------------------------------------------------

postcodes <- as.data.frame(table(unlist(fits$postcode)))
postcodes[order(postcodes$Freq, decreasing=T),]
# by postcode

postcodes <- readOGR("data/postcode-XXNN.shp") %>% st_as_sf("sf", crs<-"WGS84")
st_crs(postcodes) <- st_crs(greatbritain) # Set CRS
pc <- tm_shape(postcodes) + tm_lines(col="black", alpha=0.6)
pc

