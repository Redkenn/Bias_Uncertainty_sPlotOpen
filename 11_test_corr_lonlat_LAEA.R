library(tidyverse)
library(raster)
library(sf)
library(viridis)
library(ggplot2)
library(rnaturalearth)
# original one
d84 <- readRDS("d2.rds")




############ PLOT #############

x <- raster()
e <- extent( -180, 180, -90, 90) 
r<-raster::crop(x, e)
r <- disaggregate(r, fact=2)
vals <- 1:ncell(r)
r <- setValues(r, vals)

df.r <- as.data.frame(r, xy=TRUE)
colnames(df.r)<- c("x","y","id")

############# n Plot x grid: samplig effort ############

nPlot <- d84 %>% dplyr::select(id, PlotObservationID)%>% unique()%>% group_by(id) %>%
  mutate(nPlot = n()) %>%
  ungroup()



nPlot <- nPlot %>% inner_join(., df.r, by="id")

coordinates(nPlot)= ~x+y


nPlotr <- rasterize(nPlot, r)

saveRDS(nPlotr, "nPlotr.rds")
####### per corr
nPlot.df <- as.data.frame(nPlotr, xy= T) %>% 
  drop_na()  

set.seed(999)
v <- sample(1:1122, 500, replace = FALSE)

sub.nPlot.df <- nPlot.df[v,]








#lets try with new crs
d3035 <- readRDS("d2_LAEA.rds")

############ PLOT  3035 #############


####change crs base raster
r_reprojected <- projectRaster(r, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", method = "bilinear")
#r.lamb <- projectRaster(r, crs= "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", res= 55500)


df.r3035 <- as.data.frame(r_reprojected, xy=TRUE, na.rm = TRUE)

colnames(df.r3035)<- c("x","y","id")
############# n Plot x grid: samplig effort  3035############

nPlot3035 <- d3035 %>% dplyr::select(id, PlotObservationID)%>% unique()%>% group_by(id) %>%
  mutate(nPlot = n()) %>%
  ungroup()



nPlot3035 <- nPlot3035 %>% inner_join(., df.r3035, by="id")

coordinates(nPlot3035)= ~x+y


nPlotr3035 <- rasterize(nPlot3035, r_reprojected)


#now transform points
sp.nPlot <- SpatialPoints(coords = sub.nPlot.df[,1:2], proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
sf.nPlot <- st_as_sf(sp.nPlot)

#lambert
sf.nPlot <- st_transform(sf.nPlot, crs = st_crs(3035))

#back as spatial
transf.nPlot <- as(sf.nPlot, "Spatial")

#extract

df3035 <- extract(nPlotr3035, transf.nPlot, df= T, na.rm= T)

df_corr <- cbind(sub.nPlot.df, df3035$nPlot)
df_corr_complete <- df_corr[complete.cases(df_corr),]
##cor.test       cor 0.7496302 
##### try with TU with WGS84 #######

d84$Year <- as.numeric(d84$Year)

# difference in years from the most recent plot observation

d84$YearDif <- 2014-d84$Year

# Esponential decay of temporal information

TU1 <- function(x){
  y <- exp(-1*x)
  return(y)
}
d84$TU1 <- TU1(d84$YearDif)

TU1_5 <- function(x){
  y <- exp(-1/5*x)
  return(y)
}
d84$TU5 <- TU1_5(d84$YearDif)

TU1_25 <- function(x){
  y <- exp(-1/25*x)
  return(y)
}
d84$TU25 <- TU1_25(d84$YearDif)

############ plot temporal uncertainty median 
dtu<- d84 %>% dplyr::select(id, PlotObservationID, TU1, TU5, TU25)%>% unique()%>% group_by(id)  %>%
  mutate(median1= median(TU1)) %>%
  mutate(median5= median(TU5)) %>%
  mutate(median25= median(TU25)) %>%
  ungroup() %>%   
  dplyr::select(median1, median5, median25, id)

dtu <- dtu %>% inner_join(., df.r, by="id")

coordinates(dtu)= ~x+y
TUr <- rasterize(dtu, r)
saveRDS(TUr, "TUr.rds")
###df
TU.df <- as.data.frame(TUr, xy= T) %>% 
  drop_na()  

sub.TU.df <- TU.df[v,]

#with lambert
d3035$Year <- as.numeric(d3035$Year)

d3035$YearDif <- 2014-d3035$Year

d3035$TU1 <- TU1(d3035$YearDif)
d3035$TU5 <- TU1_5(d3035$YearDif)
d3035$TU25 <- TU1_25(d3035$YearDif)

dtu3035<- d3035 %>% dplyr::select(id, PlotObservationID, TU1, TU5, TU25)%>% unique()%>% group_by(id)  %>%
  mutate(median1= median(TU1)) %>%
  mutate(median5= median(TU5)) %>%
  mutate(median25= median(TU25)) %>%
  ungroup() %>%   
  dplyr::select(median1, median5, median25, id)

dtu3035 <- dtu3035 %>% inner_join(., df.r3035, by="id")

coordinates(dtu3035)= ~x+y
TUr3035 <- rasterize(dtu3035, r_reprojected)

#now transform points
sp.Tu <- SpatialPoints(coords = sub.TU.df[,1:2], proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
sf.Tu <- st_as_sf(sp.Tu)

sf.Tu<- st_transform(sf.Tu, crs = st_crs(3035))

#back as spatial
transf.Tu <- as(sf.Tu, "Spatial")

#extract

dfTu <- extract(TUr3035, transf.Tu, df= T, na.rm= T)
dfTu <- dfTu %>% 
  dplyr::select(median1, median5, median25)
colnames(dfTu) <- c("median1L", "median5L", "median25L")
df_corrTu <- cbind(sub.TU.df, dfTu)

df_corrTu_complete <- df_corrTu[complete.cases(df_corrTu),]
#cor.test  TU1 0.3148638 
#cor.test TU5 0.7289998
#cor.test TU25 0.7657938 
