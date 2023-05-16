library(spatialEco)
library(sf)
library(raster)
library(tidyverse)

d <- readRDS("d.rds")

y1 <- d %>% mutate(Period1 = ifelse((Year >= 1992) & (Year <= 1998), "TRUE", "FALSE")) %>% 
      filter(Period1 == "TRUE") %>%
      select(Longitude, Latitude, id)%>%
      unique()

y2 <- d %>% mutate(Period2 = ifelse((Year >= 1999) & (Year <= 2004), "TRUE", "FALSE")) %>% 
            filter(Period2 == "TRUE") %>%
            select(Longitude, Latitude, id)%>%
            unique()

y3 <- d %>% mutate(Period3 = ifelse((Year >= 2005) & (Year <= 2014), "TRUE", "FALSE")) %>% filter(Period3 == "TRUE") %>%
        filter(Period3 == "TRUE") %>%
        select(Longitude, Latitude, id)%>%
        unique()

y1id <- y1 %>% select(id) %>% unique()
y2id <- y2 %>% select(id) %>% unique()
y3id <- y3 %>% select(id) %>% unique()


y1y2 <- y1id %>% inner_join(., y2id, by="id")
y1y2y3 <- y1y2 %>% inner_join(., y3id, by="id") # 138 id

y1s <- y1%>% 
  inner_join(., y1y2y3, by="id") %>%
  dplyr::select(Longitude, Latitude, id)%>%
  unique()%>%
  mutate(across(where(is.numeric), round, 5))%>%
  group_by(id)%>%
  nest()

y2s <- y2%>%
  inner_join(., y1y2y3, by="id") %>%
  dplyr::select(Longitude, Latitude, id)%>%
  unique()%>%
  mutate(across(where(is.numeric), round, 5))%>%
  group_by(id)%>%
  nest()

y3s <- y3%>% 
  inner_join(., y1y2y3, by="id") %>%
  dplyr::select(Longitude, Latitude, id)%>%
  unique()%>%
  mutate(across(where(is.numeric), round, 5))%>%
  group_by(id)%>%
  nest()

NNI <- function(inputData=NULL){
  
  coordinates(inputData) = ~ Longitude+Latitude 
  inputData <- inputData %>% st_as_sf()
  nni <- nni(inputData, win="extent")
  
  
  return(nni)
}

map(y1s$data, ~ NNI(.x))
y1s <- .Last.value

NNI_1 <- unlist(map(y1s[1:138], "NNI"))

dfNNI_1 <- data.frame(NNI1 = NNI_1)


map(y2s$data, ~ NNI(.x))
y2s <- .Last.value

NNI_2 <- unlist(map(y2s[1:138], "NNI"))

dfNNI_2 <- data.frame(NNI2 = NNI_2)

map(y3s$data, ~ NNI(.x))
y3s <- .Last.value

NNI_3 <- unlist(map(y3s[1:138], "NNI"))

dfNNI_3 <- data.frame(NNI3 = NNI_3)

NNIf <- cbind(y1y2y3, dfNNI_1, dfNNI_2, dfNNI_3)

NNIf <- NNIf[!is.infinite(rowSums(NNIf)),] ## 71 id finali

######## in quale regione ci sono i grids che hanno plots per tutti e 3 i periodi temporali
Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe")

dTb <- d%>% inner_join(., y1y2y3, by="id") %>% dplyr::select(Longitude, Latitude, id)%>%
  unique()

coordinates(dTb) = ~ Longitude+Latitude 
Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe")

Europe <- Europe %>%
dplyr::select(geometry,name_long)  %>%
filter(name_long!='Russian Federation')

plot(st_geometry(Europe))
points(dTb, add=T)



######### boxplot time ########
library(ggplot2)

bNNIf <- NNIf %>% pivot_longer(-id, names_to = "time", values_to = "values")

bNNIf %>% 
  ggplot()+
  geom_boxplot(aes(x=time,y=values))+
  labs(y='NNI')+
  theme_classic()

####### boxplot time and region

dTb <- d%>% inner_join(., y1y2y3, by="id") %>% dplyr::select(Longitude, Latitude, id, Year)%>%
  unique()

coordinates(dTb) = ~ Longitude+Latitude 
Europe <- as(Europe, "Spatial")
crs(Europe) <- crs(dTb)
ov <- sp::over(dTb, Europe)

dTb <- d%>% inner_join(., y1y2y3, by="id") %>% dplyr::select(Longitude, Latitude, id, Year)%>%
  unique()%>% cbind(ov)

Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe")

Europe <- Europe %>%
  dplyr::select(geometry,name_long)  

dTb_Europe <- dTb %>% left_join(., Europe, by="name_long") 


y1s <- dTb_Europe %>% mutate(Period1 = ifelse((Year >= 1992) & (Year <= 1998), "TRUE", "FALSE")) %>% 
  filter(Period1 == "TRUE") %>%
  select(Longitude, Latitude, id, name_long)%>%
  unique()%>%
  mutate(across(where(is.numeric), round, 5))%>%
  group_by(id)%>%
  nest()


y2s <- dTb_Europe %>% mutate(Period2 = ifelse((Year >= 1999) & (Year <= 2004), "TRUE", "FALSE")) %>% 
  filter(Period2 == "TRUE") %>%
  select(Longitude, Latitude, id, name_long)%>%
  unique()%>%
  mutate(across(where(is.numeric), round, 5))%>%
  group_by(id)%>%
  nest()


y3s <- dTb_Europe %>% mutate(Period3 = ifelse((Year >= 2005) & (Year <= 2014), "TRUE", "FALSE")) %>% filter(Period3 == "TRUE") %>%
  filter(Period3 == "TRUE") %>%
  select(Longitude, Latitude, id, name_long)%>%
  unique()%>%
  mutate(across(where(is.numeric), round, 5))%>%
  group_by(id)%>%
  nest()

NNIfr <- NNIf%>% left_join(., dTb_Europe, by="id") %>% select(-c(Longitude, Latitude, Year, geometry)) %>% unique() %>% drop_na()

bNNIfr <- NNIfr  %>% filter(!name_long=="Germany") %>% filter(!name_long=="Croatia")%>% pivot_longer(-c(id,name_long), names_to = "time", values_to = "values")

bNNIfr %>% 
  ggplot()+
  geom_boxplot(aes(x=time,y=values, fill=name_long))+
  labs(y='NNI')+
  theme_classic()
