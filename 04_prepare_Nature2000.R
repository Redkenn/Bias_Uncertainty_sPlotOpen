library(sf)
library(rnaturalearth)
library(tidyverse)
library(raster)
library(rgdal)

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

DFinS <- df.ov_plotNat %>% filter(In_Out %in% 1) %>% group_by(id)  %>%
  mutate(countIN= sum(In_Out)) %>%
  ungroup() 

nNat2 <- DFinS %>% inner_join(., df.r, by="id") 

coordinates(nNat2)= ~x+y

rst <- raster(ext = extent(c(-180, 180, -90, 90)), crs = crs(r), res = 0.5)
nNat2r <- rasterize(nNat2, rst)


plot(nNat2)
spNat <- as(nNat2r,"SpatialPolygonsDataFrame")
world <- ne_coastline(scale = "medium", returnclass = "sf")

spNat  %>% 
  st_as_sf () %>%  
  ggplot()+
geom_sf(aes(fill =log(countIN)))+
  geom_sf(data=world,
          colour = "black", fill = "transparent")+
  scale_fill_viridis(option='viridis',direction = 1,alpha = 0.7)+
  coord_sf(xlim = c(-20, 51), ylim = c(30, 71), expand = TRUE)+
  labs(title = "Number of plots in Nature2000", x="Longitude", y="Latitude", fill = "ln nPlots") +theme_light()+
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

write_rds(plot, "nPlotr_sp.rds")
######## sarebbe figo farlo per country!!! #########



#################### stessi passaggi ma per out ##################

d2 <- read.csv("d2.csv")

d2$Year <- substr(d2$d.Date_of_recording, 1,4)
d2 <- d2 %>% filter(Year <= 1992)
y3 <- d2 %>% dplyr::select(d.Longitude, d.Latitude, d.PlotObservationID)%>%unique()
coordinates(y3)= ~d.Longitude+d.Latitude
crs(y3) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs "
Nat.sp <- readOGR("/media/data/r_projects/sampling_bias/sPlot/Natura2000", "Natura2000_end2021_epsg3035")
point <- as(y3, "sf")
Nat2000 <- as(Nat.sp, "sf")
Nat2000<- st_transform(Nat2000, crs = st_crs(point))

point.sp <- as(point,"Spatial")
Nat2000.sp <- as(Nat2000,"Spatial")
ov <- sp::over(point.sp, Nat2000.sp)

y3 <- d2 %>% dplyr::select(d.Longitude, d.Latitude, d.PlotObservationID)%>%unique()
ov$d.PlotObservationID<- y3[, 3]
ov$In_Out <- ifelse(is.na(ov$SITECODE), 0, 1)

df.ov_plotNat<- cbind(ov[,7:8], y3[,1:2])

DFin <- df.ov_plotNat %>% filter(In_Out %in% 1)   #41.67% quelli che poi sono entrati in Nat2000
DFout <- df.ov_plotNat %>% filter(In_Out %in% 0)  #58.33%


########## barplot per date

d2 <- read.csv("d2.csv")

d2$Year <- substr(d2$d.Date_of_recording, 1,4)
d2 <- d2 %>% filter(Year >= 1992)
d2.date <- d2%>%left_join(df.ov_plotNat, by = "d.PlotObservationID")%>% filter(In_Out %in% 1)
d2.date <- d2.date%>% dplyr::select(Year,d.PlotObservationID)%>%unique()
hist(as.numeric(d2.date$Year))
