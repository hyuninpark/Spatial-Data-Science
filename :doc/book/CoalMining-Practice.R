# Set working directory
setwd("/Users/maryniakolak/Desktop/WVCoal")

#install.packages("sf")
library(sf)
library(tidyverse)

# Load new census data you plan to join.
Census.Data <-read.csv("ACS_15_5YR_DP03.csv")

# Load the output area shapefiles
County.Areas<- st_read("WV_Counties.shp")

glimpse(County.Areas)
head(County.Areas)

dim(County.Areas)

glimpse(Census.Data)

