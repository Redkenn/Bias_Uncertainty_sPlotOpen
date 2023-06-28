library(sf)
library(rnaturalearth)
library(tidyverse)
library(raster)
library(rgdal)
library(SpatialPack)

d <- readRDS("d.rds")
d_NNI <- readRDS("d_NNI.rds")

nNat2 <- readRDS("nNat2.rds")
nPlot <- d %>% dplyr::select(id, PlotObservationID)%>% unique()%>% group_by(id) %>%
  mutate(nPlot = n()) %>%
  ungroup() %>% dplyr::select(id, nPlot)


x <- raster()
e <- extent( -180, 180, -90, 90) 
r<-raster::crop(x, e)
r <- disaggregate(r, fact=2)
vals <- 1:ncell(r)
r <- setValues(r, vals)

df.r <- as.data.frame(r, xy=TRUE)
colnames(df.r)<- c("x","y","id")

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

median_dist <- readRDS("new_median.rds")
pop05 <- readRDS("pop05.rds")


######## select and clean for GAM with NNI ########

nNat2 <- nNat2@data %>% inner_join(., nPlot, by="id") %>% mutate(rIN = countIN/nPlot) %>% dplyr::select(id, rIN) %>% unique()
DIS <- median_dist %>% dplyr::select(id, median_dist) %>% unique()
NNI <- d_NNI %>% dplyr::select(id, NNI) %>% unique()

NAT_DIS <- DIS %>% inner_join(., nNat2, by="id")
NAT_DIS_NNI <- NAT_DIS %>% inner_join(., NNI, by="id")


cells <- NAT_DIS_NNI$id
ctr <- as.data.frame(xyFromCell(r, cells))
coordinates(ctr)= ~x+y
crs(ctr)=crs(pop05)

Vpop05 <- extract(pop05, ctr)
Vcompr <- extract(compr, ctr)

NAT_DIS_NNI_POP <- cbind(NAT_DIS_NNI, pop=Vpop05)%>% relocate("NNI", .before = "median_dist")
NAT_DIS_NNI_POP <- NAT_DIS_NNI_POP[,-1]



########## Tjostheim’s coefficient ###########

### from line 129 means pearson correlation


NAT_DIS_NNI_POP_COMP <- cbind(NAT_DIS_NNI_POP, comp =Vcompr, id=cells)
NAT_DIS_NNI_POP_COMP <- NAT_DIS_NNI_POP_COMP %>% drop_na() 
NAT_DIS_NNI_POP_COMP <- NAT_DIS_NNI_POP_COMP[,-5]
NAT_DIS_NNI_POP_COMP <- NAT_DIS_NNI_POP_COMP %>% inner_join(., df.r, by="id")

# for completeness

corCdist <- cor.spatial(NAT_DIS_NNI_POP_COMP$comp.Completeness, NAT_DIS_NNI_POP_COMP$median_dist, NAT_DIS_NNI_POP_COMP[c("x","y")])
corCnat <- cor.spatial(NAT_DIS_NNI_POP_COMP$comp.Completeness, NAT_DIS_NNI_POP_COMP$countIN, NAT_DIS_NNI_POP_COMP[c("x","y")])
corCnat <- cor.spatial(NAT_DIS_NNI_POP_COMP$comp.Completeness, NAT_DIS_NNI_POP_COMP$pop, NAT_DIS_NNI_POP_COMP[c("x","y")])

# for NNI

corNdist <- cor.spatial(NAT_DIS_NNI_POP_COMP$NNI, NAT_DIS_NNI_POP_COMP$median_dist, NAT_DIS_NNI_POP_COMP[c("x","y")])
corNnat <- cor.spatial(NAT_DIS_NNI_POP_COMP$NNI, NAT_DIS_NNI_POP_COMP$countIN, NAT_DIS_NNI_POP_COMP[c("x","y")])
corNpop <- cor.spatial(NAT_DIS_NNI_POP_COMP$NNI, NAT_DIS_NNI_POP_COMP$pop, NAT_DIS_NNI_POP_COMP[c("x","y")])

