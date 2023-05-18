require(TPD)
require(tidyverse)
dd <- readRDS('dYear.rds')
dd %>% 
  mutate(Year=as.numeric(Year)) %>% 
  pivot_wider(names_from = Year,values_from = nPlotY) %>% 
  dplyr::select(id, PlotObservationID,order(colnames(.))) %>% 
  as.data.frame() -> ddWide

ddWide[is.na(ddWide)] <- 0

ddWide %>% 
  group_by(id) %>% 
summarise_all(.funs=function(x) sum(x,na.rm=TRUE)) %>% 
  dplyr::select(-PlotObservationID)->ddWide

head(ddWide)



# calcluate Pielou's evenness


df <- data.frame(S=rowSums(ddWide[,2:24]),
                 H=diversity(ddWide[,2:24]))
df$J <- df$H/log(df$S)
df$ID <- ddWide$id


