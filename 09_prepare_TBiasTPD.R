require(TPD)
require(tidyverse)
dd <- readRDS('dYear.rds')

dd %>% dplyr::select(-PlotObservationID)%>%
  unique%>%
  mutate(Year=as.numeric(Year)) %>% 
  pivot_wider(names_from = Year,values_from = nPlotY) %>% 
  dplyr::select(id, order(colnames(.))) %>% 
  as.data.frame() -> ddWide

ddWide[is.na(ddWide)] <- 0




# calcluate Pielou's evenness

library(vegan)

df <- data.frame(S=rowSums(ddWide[,2:24]),
                 H=diversity(ddWide[,2:24]))
df$J <- df$H/log(df$S)
df$ID <- ddWide$id


