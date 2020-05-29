##############################
# Lab 8. Points to Predictions
##############################
# July 12, 2018, updated May 27, 2020
# Compiled by M. Kolak with Source Material from:
# Lansley and Cheshire (2016): https://data.cdrc.ac.uk/tutorial/an-introduction-to-spatial-data-analysis-and-visualisation-in-r
# Kingi (2017): http://hautahi.com/rmaps
# And then adpated to an sf framework.

##############################
# Environment Setup
##############################

# Set Working Directory
setwd("~/Desktop/Lab8-CodeData")

library(sf)
library(tmap)
library(leaflet)
library(raster) # Needed for grid and kernel density surface
library(adehabitatHR) # Needed for kernel density surface

##############################
# Load and Join Data
##############################

# Load non-spatial Data (csv)
Census.Data <-read.csv("practicaldata.csv")
head(Census.Data)

# Load spatial Data (shapefile)
Output.Areas <- st_read("Camden_oa11.shp")
head(Output.Areas)

# Join our census data to the shapefile
OA.Census <- merge(Output.Areas, Census.Data, by.x="OA11CD", by.y="OA")
head(OA.Census)

# Load the house prices csv file
houses <- read.csv("CamdenHouseSales15.csv")
head(houses)

##############################
# Inspect and Prepare Data
##############################

# We only need a few columns for this practical
houses <- houses[,c(1,2,8,9)]
head(houses)

# create a House.Points SpatialPointsDataFrame using coordinates in columns 3 and 4
House.Points <- st_as_sf(houses, coords = c("oseast1m","osnrth1m"), crs = 27700)

# Plot to ensure the points were encoded correctly
plot(House.Points)

# Check coordinate reference system, transform if needed.
st_crs(OA.Census) 
st_crs(House.Points)


##############################
# Point Data Method: Basic Visualizations
##############################

# This plots a blank base map, we have set the transparency of the borders to 0.4
tm_shape(OA.Census) + tm_borders(alpha=.4) 

# Creates a color-coded dot map
tm_shape(OA.Census) + tm_borders(alpha=.4) +
  tm_shape(House.Points) + tm_dots(col = "Price", palette = "Reds", style = "quantile") 

# Rescale the points
tm_shape(OA.Census) + tm_borders(alpha=.4) +
  tm_shape(House.Points) + tm_dots(col = "Price", scale = 1.5, palette = "Reds", style = "quantile", title = "Price Paid (£)")  

# Turn on interactive Leaflet map view
tmap_mode("view")

# View interactive Map; in Export, Save as Web Page
tm_shape(OA.Census) + tm_borders(alpha=.4) +
  tm_shape(House.Points) + tm_dots(col = "Price", scale = 1.5, palette = "Reds", style = "quantile", title = "Price Paid (£)")  

# Turn off interactive view
tmap_mode("plot")


##############################
# Point Data Method: Graduate Symbol Visualizations
##############################

# Creates a graduated symbol map
tm_shape(OA.Census) + tm_borders(alpha=.4) + 
  tm_shape(House.Points) + tm_bubbles(size = "Price", col = "Price", palette = "Reds", style = "quantile", legend.size.show = FALSE, title.col = "Price Paid (£)") +
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, frame = FALSE)


##############################
# Point Data Method: Graduate Symbol with Overlay Visualizations
##############################

# Creates a graduated symbol map with overlay choropleth
tm_shape(OA.Census) + tm_fill("Unemployed", alpha=0.8, palette = "Greys", style = "quantile", title = "% Unemployed") + 
  tm_borders(alpha=.4) + 
  tm_shape(House.Points) + tm_bubbles(size = "Price", col = "Price", palette = "PuRd", style = "quantile", legend.size.show = FALSE, title.col = "Price Paid (£)", border.col = "black", border.lwd = 0.1, border.alpha = 0.1) +
  tm_layout(legend.text.size = 0.8, legend.title.size = 1.1, frame = FALSE)


##############################
# Point Data Method: Buffer Generation
##############################

# Create 200m buffers for each house point
house_buffers <- st_buffer(House.Points, 200)

# Map in tmap
tm_shape(OA.Census) + tm_borders() +
  tm_shape(house_buffers) + tm_borders(col = "blue") +
  tm_shape(House.Points) + tm_dots(col = "red") 


##############################
# Point Data Method: Buffer Count per Area
##############################

# Count buffers within each area; generates a vector of totals
count_buffers <- lengths(st_within(OA.Census, house_buffers))
head(count_buffers)

# Stick buffer totals back to the census master file
OA.Census <- cbind(OA.Census,count_buffers)
head(OA.Census)

# Map density of buffers per census area
tm_shape(OA.Census) + tm_fill(col = "count_buffers", palette = "BuGn", style = "quantile",title = "Housing Density")


##############################
# Point Data Method: Buffer Union (or Dissolve)
##############################

# Merge the Buffers
union.buffers <- st_union(house_buffers)

# Map Housing Buffers
tm_shape(OA.Census) + tm_borders() +
  tm_shape(union.buffers) + tm_fill(col = "blue", alpha = .4) + tm_borders(col = "blue") +
  tm_shape(House.Points) + tm_dots(col = "red") 


##############################
# Point Data Method: Group Attribute by Area
##############################

# Point in polygon. Gives the points the attributes of the polygons that they are in
pip <- st_join(House.Points, OA.Census, join = st_within)
head(pip)

# Aggregate average house prices by the OA11CD (OA names) column
OA <- aggregate(pip$Price, by = list(pip$OA11CD), mean)
head(OA)

# Change the column names of the aggregated data
names(OA) <- c("OA11CD", "Price")

# Join the aggregated data back to the OA.Census polygon
OA.Census <- merge(OA.Census, OA, by = "OA11CD", all.x = TRUE)

# Map mean housing price per area
tm_shape(OA.Census) + tm_fill(col = "Price", style = "quantile", title = "Mean House Price (£)")


##############################
# Point Data Method: Group by Grid
##############################

# Convert sf objects to sp objects for raster and adehabitate packages
OA.Census.sp <- sf:::as_Spatial(OA.Census)
House.Points.sp <- sf:::as_Spatial(House.Points)

# Make a Grid. First define boundaries or extent of grid
grid.extent <- extent(bbox(OA.Census.sp))        

# Next grade a raster object from the extent
grid.raster <- raster(grid.extent)            

# Specify our dimensions; split extent into 30 x 30 cells
dim(grid.raster) <- c(50,50)       

# Project the grid using our shapefile CRS
projection(grid.raster) <- CRS(proj4string(OA.Census.sp))  

# Convert into a spatial data frame
grid.sp <- as(grid.raster, 'SpatialPolygonsDataFrame') 

# Convert spatial data frame to matrix data structure
grid.matrix <- grid.sp[OA.Census.sp,]

# Aggregate housing prices by grid matrix
OA.grid <- aggregate(x=House.Points.sp["Price"], by=grid.matrix,FUN=mean)

# If there is a null value, assign 0
OA.grid$Price[is.na(OA.grid$Price)] <- 0

# Inspect data range
summary(OA.grid@data)

# Map
tm_shape(OA.grid) + tm_fill(col = "Price", style = "quantile", n = 7, title = "Mean House Price (£)") 


##############################
# Point Data Method: Kernel Density Surface
##############################

# Runs the kernel density estimation; look up the function parameters for more options
kde.output <- kernelUD(House.Points.sp, h="href", grid = 1000)

plot(kde.output)

# Converts to raster
kde <- raster(kde.output)

# Sets projection to British National Grid
projection(kde) <- CRS("+init=EPSG:27700")

# Maps the raster in tmap, "ud" is the density variable
tm_shape(kde) + tm_raster("ud")

# Creates a bounding box based on the extents of the Output.Areas polygon
bounding_box <- bbox(OA.Census.sp)

# Maps the raster within the bounding box
tm_shape(kde, bbox = bounding_box) + tm_raster("ud")

# Mask the raster by the output area polygon
masked_kde <- mask(kde, Output.Areas)

# Maps the masked raster, also maps white output area boundaries
tm_shape(masked_kde, bbox = bounding_box) + tm_raster("ud", style = "quantile", n = 100, legend.show = FALSE, palette = "YlGnBu") +
  tm_shape(Output.Areas) + tm_borders(alpha=.3, col = "white") +
  tm_layout(frame = FALSE)

# Compute homeranges for 75%, 50%, 25% of points, objects are returned as spatial polygon data frames
range75 <- getverticeshr(kde.output, percent = 75)
range50 <- getverticeshr(kde.output, percent = 50)
range25 <- getverticeshr(kde.output, percent = 25)

# the code below creates a map of several layers using tmap
tm_shape(Output.Areas) + tm_fill(col = "#f0f0f0") + tm_borders(alpha=.8, col = "white") +
  tm_shape(House.Points) + tm_dots(col = "blue") +
  tm_shape(range75) + tm_borders(alpha=.7, col = "#fb6a4a", lwd = 2) + tm_fill(alpha=.1, col = "#fb6a4a") +
  tm_shape(range50) + tm_borders(alpha=.7, col = "#de2d26", lwd = 2) + tm_fill(alpha=.1, col = "#de2d26") +
  tm_shape(range25) + tm_borders(alpha=.7, col = "#a50f15", lwd = 2) + tm_fill(alpha=.1, col = "#a50f15") +
  tm_layout(frame = FALSE)

# Write your kernel density surface to raster format
writeRaster(masked_kde, filename = "kernel_density.grd")

