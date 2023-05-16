library(tidyverse)
library(KnowBR)


#### Completeness per grids with incidence data

d <- readRDS("d.rds")
d$Species <- gsub(d$Species,pattern = '_',replacement = ' ')

d$records <- 1

# community matrix IDplots x species
d3 <- d %>%
  dplyr::select(Species, PlotObservationID, records)%>%
  pivot_wider(names_from =Species, values_from = records, id_cols=PlotObservationID, values_fn = list(records = sum), values_fill = 0)%>%
  mutate(across(where(is.numeric), round, 0)) %>% unique()
#length(unique(d$PlotObservationID))
#[1] 10501
#length(unique(d$Species))
#[1] 5441

y1 <- d %>% dplyr::select(PlotObservationID, Longitude,  Latitude) %>% unique()

# table for KnowBR format B
d3 <- merge(d3, y1, by="PlotObservationID") 


d3 <- d3[,-1]
d3 <- d3 %>% relocate(c( Longitude, Latitude), .before = "Aegopodium podagraria")

data(adworld)                    
KnowB(data=d3, save="CSV", format="B", cell=30) # 0.5 degree

#########

est <- read.csv("Estimators.CSV", header=T, sep=";")
est$Latitude <- gsub(est$Latitude,pattern = ',',replacement = '.')
est$Longitude <- gsub(est$Longitude,pattern = ',',replacement = '.')
est$Completeness <- gsub(est$Completeness,pattern = ',',replacement = '.')

comp <- est %>% dplyr::select(Longitude, Latitude, Completeness) %>% drop_na() ### drop_na() solo per plots
comp$Longitude <-as.numeric(comp[,1])
comp$Latitude <-as.numeric(comp[,2])
comp$Completeness <-as.numeric(comp[,3])

x <- raster()
e <- extent( -180, 180, -90, 90) 
r<-raster::crop(x, e)
r <- disaggregate(r, fact=2)
vals <- 1:ncell(r)
r <- setValues(r, vals)


coordinates(comp)= ~Longitude+Latitude

rst <- raster(ext = extent(c(-180, 180, -90, 90)), crs = crs(r), res = 0.5)
compr <- rasterize(comp, rst, fun=mean)


world <- ne_countries(scale = "medium", returnclass = "sf")

compr_sp <- as(compr,'SpatialPolygonsDataFrame')
compr_sp %>% 
  st_as_sf () %>%  
  ggplot()+
  geom_sf(aes(fill =Completeness))+
  geom_sf(data=world,
          colour = "black", fill = "transparent")+
  scale_fill_viridis(option='viridis',direction = 1,alpha = 0.7)+
  coord_sf(xlim = c(-20, 51), ylim = c(30, 71), expand = TRUE)+
  labs(title = "Completeness", x="Longitude", y="Latitude", fill = "Completeness") +theme_light()+
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

write_rds(plot, "compr_sp.rds")
