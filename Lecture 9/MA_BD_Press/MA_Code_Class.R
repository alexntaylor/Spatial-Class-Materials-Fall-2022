# Code for Calculating Market Access for Spatial Class
# Last edited: 10/19/22
# Author: Noel Johnson

library(tidyverse)
library(sf)
library(stargazer)
library(haven)
library(raster)

setwd("~/Documents/George Mason/2022-23 Classes/Spatial/Spatial-Class-Materials-Fall-2022/Lecture 9/MA_BD_Press")

cities_raw <- read_dta("All_Cities.dta")

# Spatialize the data
EEC <- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=43 +lat_2=62 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs"
cities <- st_as_sf(cities_raw, coords=c("longitude", "latitude"), crs=4326) %>%
  st_transform(EEC)

# keep only the variables you want
glimpse(cities)
#cities <- cities %>% dplyr::select(ANY VARIABLES YOU WANT TO KEEP)
cities <- cities %>% filter(countryname != "Oman")
cities <- cities %>% filter(countryname != "Iraq")
cities <- cities %>% filter(countryname != "Saudi Arabia")
cities <- cities %>% filter(countryname != "Yemen")

# Bring in roman roads shape file
rom_roads <- st_read("Europe_Major_Roman_Roads/MajorRomRdsProj.shp") %>%
  st_transform(EEC)
glimpse(rom_roads)
rom_roads <- rom_roads %>% dplyr::select()
#plot(rom_roads[1], col="red", reset=F)

# Bring in medieval roads shape file
med_roads <- st_read("Europe_Medieval_Trade_Routes/MedRdsProj.shp") %>%
  st_transform(EEC)
glimpse(med_roads)
med_roads <- med_roads %>% dplyr::select()
#plot(med_roads[1], col="red", reset=F)

# Bring in rivers shape file
rivers <- st_read("Rivers/Rivers_clippedF.shp") %>%
  st_transform(EEC)
glimpse(rivers)
rivers <- rivers %>% dplyr::select()
#plot(rivers[1], col="blue", reset=F)

# Bring in seas shape file
seas <- st_read("Seas/seas_conic.shp") %>%
  st_transform(EEC)
glimpse(seas)
seas <- seas %>% dplyr::select()
# plot(seas[1], col="blue", reset=F)

# # Bring in the modern country borders
# modern_countries <- st_read("Modern Countries/Modern Europe Project.shp") %>%
#   st_transform(EEC)
# # Clip the countries using bounding box based on the cities data
# modern_countries <- modern_countries %>%
#   st_crop(cities)
# glimpse(modern_countries)
# modern_countries <- modern_countries %>% dplyr::select(CntryName)
# #plot(modern_countries[2], col="NA", reset=F)

# Visualize the data
plot(rom_roads[1], col="red", reset=F)
plot(med_roads[1], col="red", add=T)
plot(rivers[1], col="blue", add=T)
plot(seas[1], col="blue", add=T)
plot(cities[4], add=T)


# Make a bounding box of the cities so you can create one a little bigger
bbox_cities <- st_bbox(cities, crs=EEC)
bbox_cities

#bbox <- st_bbox(c(xmin = -808309.6, xmax = 5825335.1, ymax = 7073868.6, 
# ymin = 3042103.4), crs = EEC)

xrange <- bbox_cities$xmax - bbox_cities$xmin # range of x values
yrange <- bbox_cities$ymax - bbox_cities$ymin # range of y values

bbox_cities[1] <- bbox_cities[1] - (0.025 * xrange) # xmin - left
bbox_cities[3] <- bbox_cities[3] + (0.025 * xrange) # xmax - right
bbox_cities[2] <- bbox_cities[2] - (0.025 * yrange) # ymin - bottom
bbox_cities[4] <- bbox_cities[4] + (0.025 * yrange) # ymax - top

bbox_cities_sf <- bbox_cities %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

bbox_cities_sf

# make grid of 50km cells
grid_sf <- bbox_cities_sf %>% st_make_grid(cellsize = 50000,
                                    what="polygons")
# add IDs to grid, make sf
grid_sf <- st_sf(id = 1:length(grid_sf),
                 geometry = grid_sf)

st_geometry(grid_sf)

# plot(grid_sf[1], col=NA, border=3, lwd=1, reset=F)
# plot(cities[4], add=T)

# Create travel costs for the travel technologies 
# (roman roads, medieval routes, rivers, seas, portage)

# find the travel cost data. The source is:
# Bairoch (early 20th c.): porters = 1; roads = 0.81; rivers = 0.21; 
# seas = 0.08; nature of estimate = cost
# for each travel tech shape file add a value column...
# fill that column with the travel cost...

# rom_roads
rom_roads$rom_rd_cost_1 = 0.81

# med_roads
med_roads$med_rd_cost_1 = 0.81

# rivers
rivers$river_cost_1 = 0.21

# seas
seas$seas_cost_1 = 0.08

# add a portage value to the grid file...
grid_sf$portage_cost_1 = 1.00

# Sequentially st_join the data sets...
grid_sf <- grid_sf %>%
  st_join(rom_roads) %>%
  st_join(med_roads) %>%
  st_join(rivers) %>%
  st_join(seas)

# make 4 different grids for cost_1, cost_2, cost_3, and cost_4...
glimpse(grid_sf)

# grid_cost_1
grid_cost_1 <- grid_sf %>%
  dplyr::select(id, portage_cost_1, rom_rd_cost_1, med_rd_cost_1, river_cost_1, seas_cost_1) %>%
  group_by(id) %>%
  summarize(least_cost_1 = min(portage_cost_1, rom_rd_cost_1, med_rd_cost_1, river_cost_1, seas_cost_1, na.rm = T))


# Make 50k raster for cost_1
## Set up a raster "template" for a 50 km grid
bbox_cities
# order for extent command is: xmin, xmax, ymin, ymax
ext <- extent(-918861.3, 3731139, 3384302, 7164302)
gridsize <- 50000
r <- raster(ext, res=gridsize)
## Rasterize the shapefile
rr_cost_1 <- rasterize(grid_cost_1, r, field="least_cost_1")
crs(rr_cost_1) <- EEC
rr_cost_1
plot(rr_cost_1)


# load in the library to calculate least cost travel paths...
library(gdistance)
library(gdata)
library(dismo)
library(rgdal)
library(sp)
library(maptools)
library(skimr)

# Make the least cost travel cost file...

# # simplify the cities file to be just coordinates of cities you want...
cities_raw <- read_dta("All_Cities.dta")
# Spatialize the data
EEC <- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=43 +lat_2=62 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs"
cities <- st_as_sf(cities_raw, coords=c("longitude", "latitude"), crs=4326) %>%
  st_transform(EEC)
# keep only the variables you want
glimpse(cities)
#cities <- cities %>% dplyr::select(ANY VARIABLES YOU WANT TO KEEP)
cities <- cities %>% filter(countryname != "Oman")
cities <- cities %>% filter(countryname != "Iraq")
cities <- cities %>% filter(countryname != "Saudi Arabia")
cities <- cities %>% filter(countryname != "Yemen")
# cities <- distinct(cities)
# cities %>% is.na()


# Cost_1
# convert rr to Formal class RasterLayer and calculate travel costs...
writeRaster(rr_cost_1, "test.tif", overwrite=T)
samplecost <- raster("test.tif")
cities_sp <- as(cities, Class = "Spatial")
proj4string(cities_sp)
projection(samplecost)
samplecitiescoords <-as.data.frame(coordinates(cities_sp))
plot(cities_sp,pch=16,col="red")
plot(samplecost,add=TRUE,alpha=.5)
transition_raster <- transition(1/samplecost, mean, 8)
x <- as.numeric(samplecitiescoords [,1])
y <- as.numeric(samplecitiescoords [,2])
coord <-matrix(c(x,y),ncol=2)
costmatrix <-costDistance(transition_raster,coord,coord)
London <-c(-8596.52623875611, 5708784.76556167)
Rome <-c(1033924.52235117, 4728994.5568946)
LondonToRome <-shortestPath(transition_raster,London,Rome,output="SpatialLines")
lines(LondonToRome,col="black")

plot(samplecost,reset=TRUE,alpha=.5)
lines(LondonToRome,col="black")

# Convert the matrix of travel costs (costmatrix) into a dataframe you can use...
#cities_raw <- read_dta("Base_Data_8-22-19.dta")
#cities_test <- cities_raw %>% filter(year==1850)
#cities_test <- cities_test %>% dplyr::select(city_jjk, Bairoch_id, longitude, latitude)
glimpse(cities)
city_id <- cities[,2]
id_vector <- pull(city_id, city_id)
costmatrix_labeled <- cbind(id_vector, costmatrix)
colnames(costmatrix_labeled) <- c("id", id_vector)
df <- as_tibble(costmatrix_labeled)

# convert from wide to long...
library(reshape2)
ma <- melt(df, id.vars=c("id"))
ma <- ma %>% rename(city_id = id)
ma <- ma %>% rename(destination = variable)
ma <- ma %>% rename(cost1 = value)

# merge in city names...
library(hablar)
#cities_raw <- read_dta("Base_Data_8-22-19.dta")
# cities_test <- cities_raw %>% filter(year==1850)
# cities_test <- cities_test %>% dplyr::select(city_jjk, Bairoch_id)
ma <- left_join(ma, cities_raw, by = "city_id")
ma <- ma %>% rename(origin = city_id)
ma <- ma %>% rename(city_id = destination)
cities <- cities %>% dplyr::select(city_jjk, city_id)
cities <- cities %>% dplyr::rename(dest_city = city_jjk)
ma <- ma %>% convert(num(city_id))
ma <- left_join(ma, cities, by = "city_id")
ma <- ma %>% rename(destination = city_id)
ma <- ma %>% rename(city_id = origin)
ma <- ma %>% rename(origin_city = city_jjk)
ma_cost1 <- ma

#












# End Code