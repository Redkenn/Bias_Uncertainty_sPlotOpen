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
dfr <- rasterize(df, rst)

## spatial polygons of world's countries
world <- ne_countries(scale = "medium", returnclass = "sf")

df_sp <- as(dfr,'SpatialPolygonsDataFrame')
df_sp %>% 
  st_as_sf () %>%  
  ggplot()+
  geom_sf(aes(fill = J))+
  geom_sf(data=world,
          colour = "black", fill = "transparent")+
  scale_fill_viridis(option='viridis',direction = 1,alpha = 0.7)+
  coord_sf(xlim = c(-13, 51), ylim = c(36, 71), expand = TRUE)+
  labs(title = "Pielou's evenness", x="Longitude", y="Latitude", fill = "J Index") +theme_light()+
 theme(legend.background=element_blank(),
          panel.grid = element_blank(),
          legend.position = 'bottom',
          plot.title = element_text(size=17,face = 'bold',hjust = 0.5),
          legend.title=element_text(size=12,face = 'bold'),
          axis.title.x = element_text(size=14,face = 'bold'),
          axis.text.x = element_text(size = 10, face = 'bold'),
          axis.title.y = element_text(size=14,face = 'bold'),
          axis.text.y = element_text(size = 10, face = 'bold'),
          axis.ticks.y=element_blank(),
          text = element_text(size=12), 
          strip.text = element_text(size=12),
          legend.text = element_text(size=12,angle = 0), 
          legend.key.size = unit(0.8, 'cm'))   ->plot


ggsave(plot = plot,
       filename = "Pielou.jpg",
       width = 10,
       height = 10,
       dpi = 600)


############# Boxplot countries ############
df <- data.frame(S=rowSums(ddWide[,2:24]),
                 H=diversity(ddWide[,2:24]))
df$J <- df$H/log(df$S)
df$id <- ddWide$id 
df <- df %>% drop_na()

df <- df %>% inner_join(., df.r, by="id")

coordinates(df)= ~x+y

Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe")
Europe <- as(Europe, "Spatial")

crs(Europe) <- crs(df)
ov <- sp::over(df, Europe)

df_EU <-  df %>% cbind(ov)

df_EU <- df_EU@data %>% dplyr::select(id, J, name_long) %>% drop_na()

df_nID <- df_EU %>%  dplyr::select(id, name_long) %>%  group_by(name_long) %>%
  mutate(nID = n()) %>%
  ungroup() %>% 
  filter(nID >= 17) %>%
  filter(!name_long=="Norway")


dfF <- df_nID %>% 
  inner_join(., df_EU, by="id")
dfF <- dfF[,-5]

p <- dfF %>% 
  ggplot()+
  geom_boxplot(aes(x=name_long.x,y=J, fill=J))+
  labs(y='Temporal evenness', x="Country")+
  theme_classic()+
  theme(axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 13),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        plot.title.position ='plot')

ggsave(plot = p,
       filename = "TB_even_box.jpg",
       width = 18,
       height = 7,
       dpi = 600)
