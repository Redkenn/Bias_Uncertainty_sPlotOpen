library(sf)
library(raster)
library(tidyverse)
library(rnaturalearth)
library(spatstat)
library(maptools)
library(rgdal)

#upload euorpean roads shapefile from GLOBIO

roads <- st_read("GRIP4_region4.shp")
roads <- roads %>% dplyr::filter(GP_RTP %in% c( "1", "2", "3"))  #filter roads in Highways, primary and secondary roads

#prepare data for spatstat denisty
r_points <- st_cast(roads, "POINT")
r_points <- sf::st_transform(r_points, crs= sf::st_crs(3035)) #need to project coordinates because spatstat deals with 2D coordinates 
r_points.sp <- as(r_points, "Spatial")

ppp<- as.ppp(r_points.sp) #conversion in spatstat object

#create a mask to compute density inside
Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe")
Europe <- Europe %>%
  dplyr::select(geometry,name_long)  %>%    
  filter(name_long!='Russian Federation')

Europe.tr <- sf::st_transform(Europe, crs= sf::st_crs(3035))
Europe.tr<- as(Europe.tr, "Spatial")
Europe.owin <- as(Europe.utm, "owin")

ppp.europe <- ppp[Europe.owin] #retain points that are inside the mask

kde <- density(ppp.europe, sigma= bw.diggle, kernel= "gaussian", positive= T) #compute density with smoothing kernel choosen by the bw.diggle method

#from im object to raster
gridded.kde <- as.SpatialGridDataFrame.im(kde) 
raster.kde <- raster(gridded.kde)

#assign the crs of the projected coordinates given at the beginning 
crs(raster.kde) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"

#now we need to reproject the coordiantes and set the same resolution of the other analysis 0.5°
#create a raster base
x <- raster()
e <- extent( -30, 54, 25, 74)
r<-raster::crop(x, e)
r <- disaggregate(r, fact=2)

#reproject the raster 
raster.kde.tr <- projectRaster(raster.kde, r)

saveRDS(raster.kde.tr, "density.rds")
