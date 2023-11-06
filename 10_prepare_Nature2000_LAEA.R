library(sf)
library(rnaturalearth)
library(tidyverse)
library(raster)
library(rgdal)
library(ggplot2)
library(viridis)

#in lon/lat
d <- readRDS("d2.rds")


y3 <- d %>% dplyr::select(Longitude, Latitude, PlotObservationID)%>%unique()
coordinates(y3)= ~Longitude+Latitude
crs(y3) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs "
Nat.sp <- readOGR("E:/progetto tesi/Natura2000", "Natura2000_end2021_epsg3035")
point <- as(y3, "sf")
Nat2000 <- as(Nat.sp, "sf")
Nat2000lonlat<- st_transform(Nat2000, crs = st_crs(point))

point.sp <- as(point,"Spatial")
Nat2000.sp <- as(Nat2000lonlat,"Spatial")
ov <- sp::over(point.sp, Nat2000.sp)

y3 <- d %>% dplyr::select(id, Longitude, Latitude, PlotObservationID)%>%unique()
ov$PlotObservationID<- y3[, 3]
ov$In_Out <- ifelse(is.na(ov$SITECODE), 0, 1)

df.ov_plotNat<- cbind(ov[,7:8], y3[,1:3])

DFin <- df.ov_plotNat %>% filter(In_Out %in% 1)    #43.31%
DFout <- df.ov_plotNat %>% filter(In_Out %in% 0)  #56.68%

x <- raster()
e <- extent( -180, 180, -90, 90) 
r<-raster::crop(x, e)
r <- disaggregate(r, fact=2)
vals <- 1:ncell(r)
r <- setValues(r, vals)
r


df.r <- as.data.frame(r, xy=TRUE)
colnames(df.r)<- c("x","y","id")

DFinS <- df.ov_plotNat %>% filter(In_Out %in% 1) %>% group_by(id)  %>%
  mutate(countIN= sum(In_Out)) %>%
  ungroup() 

nNat2 <- DFinS %>% inner_join(., df.r, by="id") 

nPlot <- d %>% dplyr::select(id, PlotObservationID)%>% unique()%>% group_by(id) %>%
  mutate(nPlot = n()) %>%
  ungroup() %>% dplyr::select(id, nPlot)

nNat2 <- nNat2%>% inner_join(., nPlot, by="id") %>% mutate(rIN = countIN/nPlot) %>% dplyr::select(id, rIN) %>% unique()

nNat2 <- nNat2 %>% inner_join(., df.r, by="id")



##### in LAEA
d3035 <- readRDS("d2_LAEA.rds")
point3035 <- st_transform(point, crs = st_crs(3035))
points3035.sp <- as(point3035, "Spatial")

ov3035 <- sp::over(points3035.sp, Nat.sp)

y.laea <- d3035 %>% dplyr::select(id, Longitude, Latitude, PlotObservationID)%>%unique()
ov3035$PlotObservationID<- y.laea[, 3]
ov3035$In_Out <- ifelse(is.na(ov3035$SITECODE), 0, 1)

df.ov_plotNat3035<- cbind(ov3035[,7:8], y.laea[,1:3])

DFin3035 <- df.ov_plotNat3035 %>% filter(In_Out %in% 1)    #percentuali uguali
DFout3035 <- df.ov_plotNat3035 %>% filter(In_Out %in% 0)

r_reprojected <- projectRaster(r, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs", method = "bilinear")

df.r.repr <- as.data.frame(r_reprojected, xy=TRUE)
colnames(df.r.repr)<- c("x","y","id")

DFinS3035 <- df.ov_plotNat3035 %>% filter(In_Out %in% 1) %>% group_by(id)  %>%
  mutate(countIN= sum(In_Out)) %>%
  ungroup() 

nNat3035 <-DFinS3035 %>% inner_join(., df.r.repr, by="id") 

nPlot3035 <- d3035 %>% dplyr::select(id, PlotObservationID)%>% unique()%>% group_by(id) %>%
  mutate(nPlot = n()) %>%
  ungroup() %>% dplyr::select(id, nPlot)

nNat3035<- nNat3035%>% inner_join(., nPlot3035, by="id") %>% mutate(rIN = countIN/nPlot) %>% dplyr::select(id, rIN) %>% unique()

nNat3035 <- nNat3035 %>% inner_join(., df.r.repr, by="id")
#make raster lambert
coordinates(nNat3035)= ~x+y


nNat3035r <- rasterize(nNat3035, r_reprojected)

#Correlation
#sample lon lat grids
set.seed(999)
v <- sample(1:637, 500, replace = FALSE)

sample <- nNat2[v,]

sp.sample <- SpatialPoints(coords = sample[, 3:4], proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
sf.sample <- st_as_sf(sp.sample)

sf.sample <- st_transform(sf.sample, crs = st_crs(3035))

transf.sample <- as(sf.sample, "Spatial")

df3035 <- extract(nNat3035r, transf.sample, df=T, na.rm= T)

df_corr <- cbind(sample, df3035$rIN)
df_corr_complete <- df_corr[complete.cases(df_corr),] #cor test 0.6971572
