library(sf)
library(rnaturalearth)
library(tidyverse)
library(raster)
library(rgdal)

DT2.oa$Species <- gsub(DT2.oa$Species,pattern = ' ',replacement = '_')
species_level <- DT2.oa[grepl("_", DT2.oa$Species),]
NOspecies_level <- DT2.oa[!grepl("_", DT2.oa$Species),] #tutte le osservazioni che non possono essere considerate


#d$d.Species <- gsub(d$d.Species,pattern = ' ',replacement = '_')

#Unione delle 2 tabelle necessarie per le analisi e selezione delle variabili
d <- merge(species_level, header.oa, by='PlotObservationID')

d <- d[d$Continent=='Europe',]
d <- d %>% filter(Location_uncertainty < 250)
d$Year <- substr(d$Date_of_recording, 1,4)
d <- d %>% filter(Year >= 1992)

saveRDS(d, "d_filtered.rds")

###### new grids with new crs ######
d <- readRDS("d_filtered.rds")

#raster for grids
x <- raster()
e <- extent( -180, 180, -90, 90)
r<-raster::crop(x, e)
r <- disaggregate(r, fact=2)
vals <- 1:ncell(r)
r <- setValues(r, vals)

r_reproject <-projectRaster(r, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", method = "bilinear")

####change crs for raster


# before over I have to change also coordinates in the dataframe
dCoord <- d%>% dplyr::select(Longitude, Latitude, PlotObservationID)
coordinates(dCoord)= ~Longitude+Latitude
crs(dCoord)<-  "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs " #original crs

d.sf <- st_as_sf(dCoord)
d.sf.laea <- sf::st_transform(d.sf, crs = sf::st_crs(3035)) #new crs of points

d.sp.laea <- as(d.sf.laea, "Spatial")
r_repr_sp <- as(r_reproject, "SpatialPolygonsDataFrame")

OV<- over(d.sp.laea, r_repr_sp)
colnames(OV) <- "id"
d<- cbind(OV, d)

write_rds(d, "d2_LAEA.rds")
