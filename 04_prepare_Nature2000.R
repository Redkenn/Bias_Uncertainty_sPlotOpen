library(sf)
library(rnaturalearth)
library(tidyverse)
library(raster)
library(rgdal)
library(ggplot2)
library(viridis)

d <- readRDS("d.rds")


y3 <- d %>% dplyr::select(Longitude, Latitude, PlotObservationID)%>%unique()
coordinates(y3)= ~Longitude+Latitude
crs(y3) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs "
Nat.sp <- readOGR("/media/data/r_projects/sampling_bias/sPlot/Natura2000", "Natura2000_end2021_epsg3035")
point <- as(y3, "sf")
Nat2000 <- as(Nat.sp, "sf")
Nat2000<- st_transform(Nat2000, crs = st_crs(point))

point.sp <- as(point,"Spatial")
Nat2000.sp <- as(Nat2000,"Spatial")
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

# number of plots inside pretected areas per grid cell

DFinS <- df.ov_plotNat %>% filter(In_Out %in% 1) %>% group_by(id)  %>%
  mutate(countIN= sum(In_Out)) %>%
  ungroup() 

# centroids of the grid cells

nNat2 <- DFinS %>% inner_join(., df.r, by="id")

# number of plots per grid cell

nPlot <- d %>% dplyr::select(id, PlotObservationID)%>% unique()%>% group_by(id) %>%
  mutate(nPlot = n()) %>%
  ungroup() %>% dplyr::select(id, nPlot)

# relative number of plots inside protected areas per grid cell

nNat2 <- nNat2@data %>% inner_join(., nPlot, by="id") %>% mutate(rIN = countIN/nPlot) %>% dplyr::select(id, rIN) %>% unique()

nNat2 <- nNat2 %>% inner_join(., df.r, by="id")

coordinates(nNat2)= ~x+y

rst <- raster(ext = extent(c(-180, 180, -90, 90)), crs = crs(r), res = 0.5)
nNat2r <- rasterize(nNat2, rst)

spNat <- as(nNat2r,"SpatialPolygonsDataFrame")
world <- ne_coastline(scale = "medium", returnclass = "sf")

spNat  %>% 
  st_as_sf () %>%  
  ggplot()+
geom_sf(data=world,
          colour = "black", fill = "lightgray")+
geom_sf(aes(fill =rIN))+
  scale_fill_viridis(option='viridis',direction = 1,alpha = 0.7)+
   coord_sf(xlim = c(-13, 42), ylim = c(36, 71), expand = TRUE)+
  labs(title = "Relative number of plots in Natura 2000 network", x="Longitude", y="Latitude", fill = " r/n Plots") +theme_light()+
  theme(legend.background=element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=12,face = 'bold',hjust = 0.5),
        legend.title=element_text(size=12,face = 'bold'),
        axis.title.x = element_text(size=12,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=12,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=12,angle = 0), 
        legend.key.size = unit(0.8, 'cm'))   ->plot
