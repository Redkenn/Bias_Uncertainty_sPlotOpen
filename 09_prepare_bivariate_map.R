
require(tidyverse)
library(biscale)
library(cowplot)
library(raster)
library(sf)
library(patchwork)
library(rnaturalearth)




##### bivariate nni evenness ########
d_NNI <- d_NNI %>% dplyr::select(id, NNI)
df_NNI <- df %>% inner_join(., d_NNI, by="id") %>% drop_na()


coordinates(df_NNI)= ~x+y
df_NNIr <- rasterize(df_NNI, rst, fun=mean)

df_NNIr <- stack(df_NNIr$J, df_NNIr$NNI)
df_NNIr<- stretch(df_NNIr, minv=0, maxv=22.18168)

df_NNI.sp <- as(df_NNIr, "SpatialPolygonsDataFrame")
df_NNI.sf <- df_NNI.sp%>%st_as_sf()

data <- bi_class(df_NNI.sf, x =NNI, y = J, style = "fisher", dim = 4)

myTheme<-theme(panel.background= element_rect(color="black", fill="white"),
               panel.grid.major = element_blank(),
               plot.title = element_text(size=15,face = 'bold',hjust = 0.5),
               legend.title=element_text(size=15),
               legend.text = element_text(size=7),
               axis.title.x = element_text(size = 15),
               axis.text.x = element_text(size = 15),
               axis.title.y = element_text(size = 15),
               axis.text.y = element_text(size = 15),
               plot.title.position ='plot')

map <- 
  ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class), color = NA, size = 0.1, show.legend = FALSE) +
  geom_sf(data=world,
          colour = "black", fill = "transparent")+
  bi_scale_fill(pal = "DkViolet2", dim = 4) +
  coord_sf(xlim = c(-25, 58), ylim = c(30, 75), expand = TRUE)+
  labs(
    title = "Spatial bias Vs temporal bias",
  ) +
  myTheme + theme(legend.position = "none")

#####

legend <- bi_legend(pal = "DkViolet2",
                    dim = 4,
                    xlab = "NNI",
                    ylab = "Pielou's evenness",
                    size = 4)



p12 <- map+ inset_element(legend, left = 0.68, bottom = 0.64, right = 1.08, top = 0.95, align_to="full")

ggsave(plot = p12,
       filename = "bivariate.jpg",
       width = 10,
       height = 10,
       dpi = 600)
