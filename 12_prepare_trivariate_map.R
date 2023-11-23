############## Trivariate map ############

library(tricolore)
library(ggtern)
##### prepare data ####
#TEMPORAL BIAS
require(TPD)
require(tidyverse)
library(vegan)
library(viridis)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(raster)

d <- readRDS("d.rds")

d$Year <- as.numeric(d$Year)

dd <- d %>% dplyr::select(id, PlotObservationID, Year) %>% unique() %>%
  group_by(id, Year) %>%
  mutate(nPlotY = n()) %>%
  ungroup()

# create wide temporal data.frame

dd %>% dplyr::select(-PlotObservationID)%>%  
  unique%>%
  mutate(Year=as.numeric(Year)) %>% 
  pivot_wider(names_from = Year,values_from = nPlotY) %>% 
  dplyr::select(id, order(colnames(.))) %>% 
  as.data.frame() -> ddWide

ddWide[is.na(ddWide)] <- 0




# calcluate Pielou's evenness

df <- data.frame(S=rowSums(ddWide[,2:24]),
                 H=diversity(ddWide[,2:24]))
df$J <- df$H/log(df$S)
df$id <- ddWide$id 
df <- df %>% drop_na()

### plot J index ####


x <- raster()
e <- extent( -180, 180, -90, 90) 
r<-raster::crop(x, e)
r <- disaggregate(r, fact=2)
vals <- 1:ncell(r)
r <- setValues(r, vals)

# data.frame with pixels id and coordinates
df.r <- as.data.frame(r, xy=TRUE)
colnames(df.r)<- c("x","y","id")

df <- df %>% inner_join(., df.r, by="id") # join pixels coordinates

coordinates(df)= ~x+y

rst <- raster(ext = extent(c(-180, 180, -90, 90)), crs = crs(r), res = 0.5)
TBraster <- rasterize(df, rst)

###### Taxonomic bias ####

est <- read.csv("Estimators.CSV", header=T, sep=";")
est$Latitude <- gsub(est$Latitude,pattern = ',',replacement = '.')
est$Longitude <- gsub(est$Longitude,pattern = ',',replacement = '.')
est$Completeness <- gsub(est$Completeness,pattern = ',',replacement = '.')

comp <- est %>% dplyr::select(Longitude, Latitude, Completeness) %>% drop_na() ### drop_na() solo per plots
comp$Longitude <-as.numeric(comp[,1])
comp$Latitude <-as.numeric(comp[,2])
comp$Completeness <-as.numeric(comp[,3])

coordinates(comp)= ~Longitude+Latitude

rst <- raster(ext = extent(c(-180, 180, -90, 90)), crs = crs(r), res = 0.5)
compr <- rasterize(comp, rst, fun=mean)

#### NNI 
NNI.NNI_SES <- readRDS("NNI.NNI_SES.rds")
NNI <- NNI.NNI_SES %>% dplyr::select(id, nni) %>% unique()
cells <- NNI$id
ctr <- as.data.frame(xyFromCell(r, cells))
coordinates(ctr)= ~x+y
crs(ctr)=crs(compr)

Vcompr <- extract(compr, ctr)

NNI_COMP <- cbind(NNI, Vcompr) %>% 
            drop_na() %>% 
            dplyr::select(id, nni, Completeness)

NNI_COMP_EV <- NNI_COMP %>% inner_join(., as.data.frame(df), by="id")%>% dplyr::select(id, nni, Completeness, J) %>% inner_join(., df.r, by="id")
####### to scale all to the same range ###
# z-values
NNI_COMP_EV_standardized <- NNI_COMP_EV %>% 
  mutate(across(c("nni", "Completeness","J") , scale)) %>% 
  mutate(across(c("nni", "Completeness", "J"), function(x) (x - min(x)) / (max(x) - min(x))))


######
coordinates(NNI_COMP_EV_standardized)= ~x+y

rst <- raster(ext = extent(c(-180, 180, -90, 90)), crs = crs(r), res = 0.5)
NNI_COMP_EVsr <- rasterize(NNI_COMP_EV_standardized, rst)
names(NNI_COMP_EVsr)<- c("ID","id","NNI","Completeness","Evenness")


NNI_COMP_EVssp <- as(NNI_COMP_EVsr,'SpatialPolygonsDataFrame')

tric <- Tricolore(NNI_COMP_EVssp@data, p1 = 'NNI', p2 = 'Completeness', p3 = 'Evenness',
                   contrast = 0.5, hue= 1)
NNI_COMP_EVssp@data$rgb <- tric$rgb

world <- ne_countries(scale = "medium", returnclass = "sf")

NNI_COMP_EVssp %>% 
  st_as_sf () %>%  
  ggplot()+
  geom_sf(data=world,
          colour = "black", fill = "lightgray")+
  geom_sf(aes(fill = factor(rgb)), color= "transparent")+
  scale_fill_identity()+
  coord_sf(xlim = c(-13, 42), ylim = c(36, 71), expand = TRUE)+
  labs(title = "NNI vs Completeness vs Evennesss", x="Longitude", y="Latitude", fill = "Accuracy") +theme_light()+
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
        legend.key.size = unit(0.8, 'cm'))   -> TRI

#p <- TRI +
   #annotation_custom(
   #ggplotGrob(tric$key),  xmin = -10, xmax = 20, ymin = 60, ymax )

#p <- grid.arrange(arrangeGrob(TRI, ncol=1, nrow=1),
           #arrangeGrob(tric$key, ncol=2, nrow=1), heights=c(2,1))

ggplot2::ggsave(TRI, filename= "TRI.png", width = 18, height = 7, dpi= 600)
 
legend <- tric$key

ggplot2::ggsave(legend, filename= "legend.png", width = 18, height = 7, dpi=600)
