

# Great page for learning vizualisation of maps: http://rstudio.github.io/leaflet/ 

library(ggmap)
library(tidyverse)
library(rgdal)
library(sf)
library(leaflet)
library(readxl)


setwd("~/Desktop/MSc Economics and Business Adm. in Business Intelligence/Business Intelligence/Data Science Project/Coffee project/Data/DST population")


data = read_xlsx("Coordinate of the sample cities.xlsx")

# Removes the string patter "" - OBS. have to use '' when I want to remove "". 
data$Longitude = data$Longitude%>% str_replace('"','')
data$Latitude = data$Latitude%>% str_replace('"','')

str(data)

data$Longitude = as.numeric(data$Longitude)
data$Latitude = as.numeric(data$Latitude)

# First number needs to be latitudenal data, while the second is longitudanal. 
data.SP = SpatialPointsDataFrame(data[,c(2,3)]) # if i had a negative coordinates i would have to add this data[,-c(2,3)])

Map = leaflet() %>% 
  addTiles() %>%
  addMarkers(data=data, lng = ~Longitude, lat = ~Latitude, popup = data$City)


Map


# Creating rings around the cities according to their sample size. 

Cmap = leaflet(data) %>% addTiles() %>%
  addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1,
             radius = ~sqrt(data$Sample) * 1000, popup = ~City)

Cmap









