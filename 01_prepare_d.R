library(sf)
library(rnaturalearth)
library(tidyverse)
library(raster) 
library(rgdal) 

######### join sPlotOpen datasets ##########
# first: loadding sPlotOpen .RData

DT2.oa$Species <- gsub(DT2.oa$Species, pattern = ' ', replacement = '_')
species_level <- DT2.oa[grepl("_", DT2.oa$Species),]
NOspecies_level <- DT2.oa[!grepl("_", DT2.oa$Species),] 

d <- merge(species_level, header.oa, by='PlotObservationID')

# filtering for Europe, Year >= 1992 and Location uncertainty < 250

d <- d[d$Continent=='Europe',]
d <- d %>% filter(Location_uncertainty < 250)
d$Year <- substr(d$Date_of_recording, 1,4)
d <- d %>% filter(Year >= 1992)

#length(unique(d$PlotObservationID))
#[1] 10501

# adding id values of grids with 0.5 degree of spatial resolution

Europe <- ne_countries(continent = "Europe", returnclass = "sp")
bbox <- st_bbox(Europe)
polygon <- st_as_sfc(bbox)

#hexabin
size<- 0.5


polygon.sp<- as(polygon, "Spatial")
hex_points <- spsample(polygon.sp, type = "hexagonal", cellsize= size)
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = size)

dCoord <- d%>% dplyr::select(Longitude, Latitude, PlotObservationID)
coordinates(dCoord)= ~Longitude+Latitude
crs(dCoord)<-  "+proj=longlat +datum=WGS84 +no_defs "
OV<- as.data.frame(over(dCoord, hex_grid))
colnames(OV) <- "id"
d<- cbind(OV, d)

hex_grif_sf <- st_as_sf(hex_grid)
hex_grif_sf$id <- 1:131040
