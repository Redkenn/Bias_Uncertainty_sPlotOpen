 NNI

library(spatialEco)

d <- readRDS("d.rds")

y4 <- d%>% 
  dplyr::select(Longitude, Latitude, id)%>%
  unique()%>%
  group_by(id)%>%
  nest()


NNI <- function(inputData=NULL){
  
  coordinates(inputData) = ~ Longitude+Latitude 
  inputData <- inputData %>% st_as_sf()
  nni <- nni(inputData, win="extent")
  
  
  return(nni)
}

map(y4$data, ~ NNI(.x))
y5 <- .Last.value


NNI <- unlist(map(y5[1:1122], "NNI"))

dfNNI <- NNI %>%
  as.data.frame()
#rownames_to_column(var="id")
colnames(dfNNI) <- "NNI"
dfNNI <- dfNNI%>%
  mutate(range=cut(NNI, c(0,1,Inf), include.lowest=TRUE, labels=FALSE))


d_id <- d %>% dplyr::select(id) %>% unique()

dfNNI <- cbind(dfNNI, id=d_id)
dfNNI <- dfNNI[!is.infinite(rowSums(dfNNI)),]
d_NNI <- d %>% inner_join(., dfNNI, by="id")


##### plot range #####

x <- raster()
e <- extent( -180, 180, -90, 90) 
r<-raster::crop(x, e)
r <- disaggregate(r, fact=2)
vals <- 1:ncell(r)
r <- setValues(r, vals)
r


df.r <- as.data.frame(r, xy=TRUE)
colnames(df.r)<- c("x","y","id")

d_NNIs <- d_NNI%>% dplyr::select(id, range)
d_NNIs <- d_NNIs %>% inner_join(., df.r, by="id") %>% unique()
coordinates(d_NNIs)= ~x + y 

rst <- raster(ext = extent(c(-180, 180, -90, 90)), crs = crs(r), res = 0.5)
NNIr <- rasterize(d_NNIs, rst) # perchè fun = last


world <- ne_countries(scale = "medium", returnclass = "sf")

NNIr_sp <- as(NNIr,'SpatialPolygonsDataFrame')
NNIr_sp %>% 
  st_as_sf () %>%  
  ggplot()+
  geom_sf(aes(fill = factor(range)))+
  geom_sf(data=world,
          colour = "black", fill = "transparent")+
  scale_fill_manual(values = c("#56B4E9","#E69F00"), labels=c("0.0-1.0","> 1"))+
  coord_sf(xlim = c(-20, 51), ylim = c(30, 71), expand = TRUE)+
  labs(title = "Average Nearest Neighbor Index", x="Longitude", y="Latitude", fill = "NNI") +theme_light()+
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

write_rds(plot, "NNIr_sp.rds")


####### prepare rds removing inf ######

write_rds(d_NNI, "d_NNI.rds")
