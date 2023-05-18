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
  geom_sf(aes(fill =log(nPlot)))+
  geom_sf(data=world,
          colour = "black", fill = "transparent")+
  scale_fill_viridis(option='viridis',direction = 1,alpha = 0.7)+
  coord_sf(xlim = c(-20, 51), ylim = c(30, 71), expand = TRUE)+
  labs(title = "Number of plots", x="Longitude", y="Latitude", fill = "ln nPlots") +theme_light()+
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
  mutate(median1= median(TU1)) %>%
  mutate(median5= median(TU5)) %>%
  mutate(median25= median(TU25)) %>%
  ungroup() %>%   
  dplyr::select(median1, median5, median25, id)

dtu <- dtu %>% inner_join(., df.r, by="id")

coordinates(dtu)= ~x+y

rst <- raster(ext = extent(c(-180, 180, -90, 90)), crs = crs(r), res = 0.5)
TUr <- rasterize(dtu, rst)

world <- ne_countries(scale = "medium", returnclass = "sf")

TU25r_sp <- as(TUr$median25,'SpatialPolygonsDataFrame')
TU25r_sp %>% 
  st_as_sf () %>%  
  ggplot()+
  geom_sf(aes(fill = median25))+
  geom_sf(data=world,
          colour = "black", fill = "transparent")+
  scale_fill_viridis(option='viridis',direction = 1,alpha = 0.7)+
  coord_sf(xlim = c(-20, 51), ylim = c(30, 71), expand = TRUE)+
  labs(title = "Temporal Uncertainty z = -1/25", x="Longitude", y="Latitude", fill = "Accuracy") +theme_light()+
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
        legend.key.size = unit(0.8, 'cm'))   ->plot25

saveRDS(plot25, "TU25r_sp.rds")

TU5r_sp <- as(TUr$median5,'SpatialPolygonsDataFrame')
TU5r_sp %>% 
  st_as_sf () %>%  
  ggplot()+
  geom_sf(aes(fill = median5))+
  geom_sf(data=world,
          colour = "black", fill = "transparent")+
  scale_fill_viridis(option='viridis',direction = 1,alpha = 0.7)+
  coord_sf(xlim = c(-20, 51), ylim = c(30, 71), expand = TRUE)+
  labs(title = "Temporal Uncertainty z = -1/5", x="Longitude", y="Latitude", fill = "Accuracy") +theme_light()+
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
        legend.key.size = unit(0.8, 'cm'))   ->plot5

write_rds(plot5, "TU5r_sp.rds")


TU1r_sp <- as(TUr$median1,'SpatialPolygonsDataFrame')
TU1r_sp %>% 
  st_as_sf () %>%  
  ggplot()+
  geom_sf(aes(fill = median1))+
  geom_sf(data=world,
          colour = "black", fill = "transparent")+
  scale_fill_viridis(option='viridis',direction = 1,alpha = 0.7)+
  coord_sf(xlim = c(-20, 51), ylim = c(30, 71), expand = TRUE)+
  labs(title = "Temporal Uncertainty z = -1", x="Longitude", y="Latitude", fill = "Accuracy") +theme_light()+
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
        legend.key.size = unit(0.8, 'cm'))   ->plot1

write_rds(plot1, "TU1r_sp.rds")


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

s1<- GGstack_TU %>%
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = value)) +
  facet_wrap(~ variable, nrow = 3) + 
  geom_sf(data=world,
          colour = "black", fill = "transparent", size=0.3)+  
  scale_fill_viridis(option='viridis',direction = 1,alpha = 0.7, limits=c(2.789468e-10,1))+
  labs(x="Longitude",y="Latitude", fill="Median")+
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
  coord_sf(xlim = c(-26, 41), ylim = c(32, 72), expand = TRUE)


############ plot temporal uncertainty weighted mean ############

dtu1 <- d %>% dplyr::select(id, PlotObservationID, TU1)%>% unique() %>% group_by(id, TU1) %>% mutate(nTU = n()) %>%
  ungroup() %>% dplyr::select(PlotObservationID, nTU)

nPlot <- d %>% dplyr::select(id, PlotObservationID)%>% unique()%>% group_by(id) %>%
  mutate(nPlot = n()) %>%
  ungroup() %>% dplyr::select(nPlot, PlotObservationID)

dtu <- d %>% dplyr::select(id, PlotObservationID, TU1, TU5, TU25)%>% 
  unique() %>% inner_join(., dtu1, by="PlotObservationID") %>% 
  inner_join(., nPlot, by="PlotObservationID") %>% 
  mutate(rTU=nTU/nPlot)%>%
  dplyr::select(id, rTU, TU1, TU5, TU25)%>% unique() 


  
wmdTU <- dtu %>% group_by(id) %>%
  mutate(wmTU1= weighted.mean(TU1, rTU)) %>%
  mutate(wmTU5= weighted.mean(TU5, rTU)) %>%
  mutate(wmTU25= weighted.mean(TU25, rTU)) %>%
  ungroup() %>%   
           dplyr::select(wmTU1, wmTU5, wmTU25, id)
  
  wmdtu <- wmdTU %>% inner_join(., df.r, by="id")
  
  coordinates(wmdtu)= ~x+y
  
  rst <- raster(ext = extent(c(-180, 180, -90, 90)), crs = crs(r), res = 0.5)
  TUr <- rasterize(wmdtu, rst)
  
  
  stack_wmTU <- stack(TUr$wmTU1, TUr$wmTU5, TUr$wmTU25)
  
  stack_wmTU <-
    as.data.frame(stack_wmTU, xy = TRUE) %>%
    na.omit()
  
  GGstack_wmTU <- 
    stack_wmTU %>%
    pivot_longer(
      c(-x, -y),
      names_to = "variable",
      values_to = "value")
  
  GGstack_wmTU$variable <- as.factor(GGstack_wmTU$variable)
  GGstack_wmTU <- GGstack_wmTU %>%
    mutate(variable = str_replace_all(variable, c("wmTU1" = "z=-1", "wmTU5" = "z=-1/5", "wmTU25" = "z=-1/25")))
  
  
  world <- ne_coastline(scale = "medium", returnclass = "sf")
  
  s2<- GGstack_wmTU %>%
    ggplot() +
    geom_tile(aes(x = x, y = y, fill = value)) +
    facet_wrap(~ variable, nrow = 3) + 
    geom_sf(data=world,
            colour = "black", fill = "transparent", size=0.3)+  
    scale_fill_viridis(option='viridis',direction = 1,alpha = 0.7, limits=c(2.789468e-10,1))+
    labs(x="Longitude",y="Latitude", fill="Weighted mean")+
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
    coord_sf(xlim = c(-26, 41), ylim = c(32, 72), expand = TRUE)

  
  ggsave(plot = s1,
                 filename = "medianTU.jpg",
                 width = 20,
                 height = 20,
                 dpi = 600)
  ggsave(plot = s2,
                  filename = "wmTU.jpg",
                  width = 20,
                  height = 20,
                  dpi = 600)  
  
