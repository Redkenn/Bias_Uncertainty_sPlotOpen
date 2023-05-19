library(sf)
library(rnaturalearth)
library(tidyverse)
library(raster)
library(rgdal)
library(mgcv)
library(SpatialPack)


d_NNI <- readRDS("d_NNI.rds")

nNat2 <- readRDS("nNat2.rds")


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

NAT <- nNat2@data %>% dplyr::select(id, countIN) %>% unique()
DIS <- median_dist %>% dplyr::select(id, median_dist) %>% unique()
NNI <- d_NNI %>% dplyr::select(id, NNI) %>% unique()

NAT_DIS <- DIS %>% inner_join(., NAT, by="id")
NAT_DIS_NNI <- NAT_DIS %>% inner_join(., NNI, by="id")


cells <- NAT_DIS_NNI$id
ctr <- as.data.frame(xyFromCell(r, cells))
coordinates(ctr)= ~x+y
crs(ctr)=crs(pop05)

Vpop05 <- extract(pop05, ctr)

NAT_DIS_NNI_POP <- cbind(NAT_DIS_NNI, pop=Vpop05)%>% relocate("NNI", .before = "median_dist")
NAT_DIS_NNI_POP <- NAT_DIS_NNI_POP[,-1]

######## select and clean for GAM with comp ########

cells <- NAT_DIS$id
ctr <- as.data.frame(xyFromCell(r, cells))
coordinates(ctr)= ~x+y
crs(ctr)=crs(pop05)

Vpop05 <- extract(pop05, ctr)

NAT_DIS_POP <- cbind(NAT_DIS, pop=Vpop05)



Vcompr <- extract(compr, ctr)

NAT_DIS_POP_COMP <- cbind(NAT_DIS_POP, comp =Vcompr)
NAT_DIS_POP_COMP <- NAT_DIS_POP_COMP %>% drop_na() %>% relocate("comp.Completeness", .before = "median_dist")
NAT_DIS_POP_COMP <- NAT_DIS_POP_COMP[,-c(1,6)]

############# GAM ############
NNI <- 1
pred_cols <- 2:4

names(NAT_DIS_NNI_POP)[NNI]
names(NAT_DIS_NNI_POP)[pred_cols]

form_gamNNI <- as.formula(paste0(names(NAT_DIS_NNI_POP)[NNI], "~", paste0("s(", names(NAT_DIS_NNI_POP)[pred_cols], ")", collapse = "+")))
ModelNNI <- gam(form_gamNNI, data = NAT_DIS_NNI_POP)

#R-sq.(adj) =  0.201   Deviance explained = 21.9%

COMP <- 1
pred_cols <- 2:4

names(NAT_DIS_POP_COMP)[COMP]
names(NAT_DIS_POP_COMP)[pred_cols]

form_gamCOMP <- as.formula(paste0(names(NAT_DIS_POP_COMP)[COMP], "~", paste0("s(", names(NAT_DIS_POP_COMP)[pred_cols], ")", collapse = "+")))
ModelCOMP <- gam(form_gamCOMP, data = NAT_DIS_POP_COMP)

#R-sq.(adj) =  0.352   Deviance explained = 36.3%



######## RF ########
library(ranger)
ModelRF_NNI<-ranger(NAT_DIS_NNI_POP$NNI ~., data= NAT_DIS_NNI_POP, importance='impurity') 
ModelRF_COMP<-ranger(NAT_DIS_POP_COMP$comp.Completeness ~., data= NAT_DIS_POP_COMP, importance='impurity')

# ModelRF_COMP[["r.squared"]]
#[1] 0.3094125

#ModelRF_NNI[["r.squared"]]
#[1] 0.2005293


################ PEARSON'S CORRELATION ###############

NAT_DIS_NNI_POP <- cbind(NAT_DIS_NNI, pop=Vpop05)%>% relocate("NNI", .before = "median_dist")

ctr <- as.data.frame(xyFromCell(r, cells))
coordinates(ctr)= ~x+y
crs(ctr)=crs(compr)

Vcompr <- extract(compr, ctr)

NAT_DIS_NNI_POP_COMP <- cbind(NAT_DIS_NNI_POP, comp =Vcompr)
NAT_DIS_NNI_POP_COMP <- NAT_DIS_NNI_POP_COMP %>% drop_na() 
NAT_DIS_NNI_POP_COMP <- NAT_DIS_NNI_POP_COMP[,-5]
colnames(NAT_DIS_NNI_POP_COMP) <- c("NNI","Road distance","Plots in Nature2000", "Population density", "Completeness") 

install.packages("ggcorrplot")
library(ggcorrplot)
IDcor <- round(cor(NAT_DIS_NNI_POP_COMP),4)
p1 <- ggcorrplot(IDcor, type = "upper",  lab = T, lab_col = "white", lab_size = 6, outline.color = "white", title = "Pearson's correlation - variables per grid", colors = c("#440154", "#21918C", "#FDE725"))+ ggplot2::labs(fill= "Correlation", x= c("NNI", "Road distance", "Plots in Nature2000", "Population density"), y=c("Road distance", "Plots in Nature2000", "Population density", "Completeness"))

ggsave(plot = p1,
       filename = "corr.jpg",
       width = 8,
       height = 8,
       dpi = 600)


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

