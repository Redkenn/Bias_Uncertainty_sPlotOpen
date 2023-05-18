library(sf)
library(rnaturalearth)
library(tidyverse)
library(raster)
library(rgdal)

## join sPlotOpen datasets

DT2.oa$Species <- gsub(DT2.oa$Species,pattern = ' ',replacement = '_')
species_level <- DT2.oa[grepl("_", DT2.oa$Species),]
NOspecies_level <- DT2.oa[!grepl("_", DT2.oa$Species),] 


#d$d.Species <- gsub(d$d.Species,pattern = ' ',replacement = '_')
d <- merge(species_level, header.oa, by='PlotObservationID')

# filtering for Europe, Year >= 1992 and Location uncertainty < 250

d <- d[d$Continent=='Europe',]
d <- d %>% filter(Location_uncertainty < 250)
d$Year <- substr(d$Date_of_recording, 1,4)
d <- d %>% filter(Year >= 1992)

#length(unique(d$PlotObservationID))
#[1] 10501

# adding id values of grids with 0.5 degree of spatial resolution

x <- raster()
e <- extent( -180, 180, -90, 90)
r<-raster::crop(x, e)
r <- disaggregate(r, fact=2)
vals <- 1:ncell(r)
r <- setValues(r, vals)
r
r.sp <- as(r, "SpatialPolygonsDataFrame")
crs(r.sp) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs "

dCoord <- d%>% dplyr::select(Longitude, Latitude, PlotObservationID)
coordinates(dCoord)= ~Longitude+Latitude
crs(dCoord)<-  "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs "
OV<- over(dCoord, r.sp)
colnames(OV) <- "id"
d<- cbind(OV, d)


