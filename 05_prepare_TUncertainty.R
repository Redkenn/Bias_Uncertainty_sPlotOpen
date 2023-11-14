library(tidyverse)
library(raster)
library(sf)
library(viridis)
library(ggplot2)
library(rnaturalearth)

d <- readRDS("d.rds")

d$Year <- as.numeric(d$Year)

# difference in years from the most recent plot observation

d$YearDif <- 2014-d$Year

# Esponential decay of temporal information

TU1 <- function(x){
  y <- exp(-1*x)
  return(y)
}

d$TU1 <- TU1(d$YearDif)

TU1_5 <- function(x){
  y <- exp(-1/5*x)
  return(y)
}

d$TU5 <- TU1_5(d$YearDif)

TU1_25 <- function(x){
  y <- exp(-1/25*x)
  return(y)
}

d$TU25 <- TU1_25(d$YearDif)

############ mutilines plot #######

p <- ggplot() +
  geom_line(data = d, aes(x = YearDif, y = TU25), color = "blue") +
  geom_line(data = d, aes(x = YearDif, y = TU5), color = "red") +
  geom_line(data = d, aes(x = YearDif, y = TU1), color = "black") +
  labs(title = "Curve of temporal decay", x = "Years of distance", y = "Temporal precision")

ggsave(plot = p,
       filename = "curveTU.jpg",
       width = 5,
       height = 5,
       dpi = 600)
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

nPlot <- d %>% dplyr::select(id, PlotObservationID)%>% unique()%>% group_by(id) %>%
 mutate(nPlot = n()) %>%
  ungroup()



nPlot <- nPlot %>% inner_join(., df.r, by="id")

coordinates(nPlot)= ~x+y

rst <- raster(ext = extent(c(-180, 180, -90, 90)), crs = crs(r), res = 0.5)
nPlotr <- rasterize(nPlot, rst)


world <- ne_countries(scale = "medium", returnclass = "sf")

nPlotr_sp <- as(nPlotr,'SpatialPolygonsDataFrame')
nPlotr_sp %>% 
  st_as_sf () %>%  
  ggplot()+
  geom_sf(aes(fill =log10(nPlot)))+
  geom_sf(data=world,
          colour = "black", fill = "transparent")+
  scale_fill_viridis(option='viridis',direction = 1,alpha = 0.7)+
  coord_sf(xlim = c(-20, 51), ylim = c(30, 71), expand = TRUE)+
  labs(title = "Number of plots", x="Longitude", y="Latitude", fill = "log10 Plots") +theme_light()+
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


############ plot temporal uncertainty median ############

dtu<- d %>% dplyr::select(id, PlotObservationID, TU1, TU5, TU25)%>% unique()%>% group_by(id)  %>%
  mutate(median1= median(1-TU1)) %>%
  mutate(median5= median(1-TU5)) %>%
  mutate(median25= median(1-TU25)) %>%
  ungroup() %>%   
  dplyr::select(median1, median5, median25, id)

dtu <- dtu %>% inner_join(., df.r, by="id")

coordinates(dtu)= ~x+y

rst <- raster(ext = extent(c(-180, 180, -90, 90)), crs = crs(r), res = 0.5)
TUr <- rasterize(dtu, rst)


####### stack with same scale of values #######

stack_TU <- stack(TUr$median1, TUr$median5, TUr$median25)

stack_TU <-
  as.data.frame(stack_TU, xy = TRUE) %>%
  na.omit()

GGstack_TU <- 
  stack_TU %>%
  pivot_longer(
    c(-x, -y),
    names_to = "variable",
    values_to = "value")

GGstack_TU$variable <- as.factor(GGstack_TU$variable)

world <- ne_coastline(scale = "medium", returnclass = "sf")

GGstack_TU <- GGstack_TU %>%
  mutate(variable = str_replace_all(variable, c("median1" = "z=-1", "median5" = "z=-1/5", "median25" = "z=-1/25")))

# Order the levels of the variable column
GGstack_TU$variable <- factor(GGstack_TU$variable, levels = c("z=-1", "z=-1/5", "z=-1/25"))


s1<- GGstack_TU %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = value)) +
  facet_wrap(~ variable, nrow = 1) + 
  geom_sf(data=world,
          colour = "black", fill = "transparent", size=0.3)+  
  scale_fill_viridis(option='viridis',direction = 1,alpha = 0.7, limits=c(0, 1.0000000))+
  labs(x="Longitude",y="Latitude", fill="Uncertainty (median)")+
  theme_light()+
  theme_light()+
    theme(
      legend.position = "bottom",  
      plot.title = element_text(size=30,face = 'bold',hjust = 0.5),
      legend.title=element_text(size=20,face = 'bold'),
      legend.text = element_text(size=18,face = 'bold'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.title.x = element_text(size=19,face = 'bold'),
      axis.text.x = element_text(size = 19, face = 'bold'),
      axis.title.y = element_text(size=23,face = 'bold'),
      axis.text.y = element_text(size = 19, face = 'bold'),
      axis.ticks.y=element_blank(),
      strip.text = element_text(size = 18,face = 'bold', colour = "black"))+
    guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, barwidth = 21, barheight = 1.9),
           size = guide_legend(title.position="top", title.hjust = 0.5))+
    coord_sf(xlim = c(-13, 42), ylim = c(36, 71), expand = TRUE)


  
  ggsave(plot = s1,
                 filename = "medianTU.jpg",
                 width = 20,
                 height = 20,
                 dpi = 600)

