#Show locations of nanoindenter measurement on heat map of depth 
library(tidyverse)
library(here)
library(viridis)
library(plotly)
#use map scan to generate heat map
mapDir <- here("..", "Surface mapping cut PE","Processed", "MapData")
mapFile <- file.path(mapDir,"UV00_050223_500_S1_map_Processed.csv")
mapErr <- 5 #[um]

mapData <- read.csv(mapFile)

mapData <- mapData %>%
  rename(x = X.mm.,
         y = Y.mm.,
         depth = depth.um.) 

mapData <- mapData%>%
  select(c(x,y,depth)) %>%
  filter(y <= 3)

xRange <- range(mapData$x, na.rm = TRUE)
yRange <- range(mapData$y, na.rm = TRUE)
aspectRatio <- diff(yRange) / diff(xRange)

p <- ggplot(mapData, aes(x = x, y = y, fill = depth)) +
  geom_raster( interpolate = TRUE) + 
  scale_fill_viridis(option = "deep", direction = -1) +
  geom_contour(aes(z = depth), binwidth = 10, color = "white") +
  coord_fixed(ratio = aspectRatio) +
  theme_minimal() + 
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14)) +
  labs(x = "X", 
       y = "Y", 
       fill = "Depth",
       title = "Depth with 10 um contour")
ggplotly(p)
