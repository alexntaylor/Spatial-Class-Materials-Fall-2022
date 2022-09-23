# Homework 3, Part 3
# Created: 9/23/22
# Last Edited: 9/23/22
# Author: Alexander Taylor


# Clear Workspace ---------------------------------------------------------

rm(list = ls())

# Libraries & Working Directory -------------------------------------------

pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
  return("OK")
}

global.libraries <- c("tidyverse", "sf", "haven", "raster", "gstat", "tmap", "units", "pacman", "rgdal")

results <- sapply(as.list(global.libraries), pkgTest)

library(pacman)
p_load(raster, sf, rgdal, tidyverse, haven, gstat, tmap, units)

dir <- "/Users/atay508/Documents/George Mason/2022-23 Classes/Spatial/Spatial-Class-Materials-Fall-2022/Coding Exercise 3"

setwd(dir)
getwd()


# Problem 1 ---------------------------------------------------------------

# Data
lights <- raster("./data/F142000.tif")
area <- raster("./data/land_area.tif")
kenya_county <- st_read("./data/kenya_counties.shp")

# Convert kenya_county to Spatial
kenya_county_sp <- kenya_county %>% 
  as("Spatial")

# Stack, Crop, Mask
kenya_stack <- stack(lights, area)
kenya_stack <- crop(kenya_stack, kenya_county_sp)
kenya_stack <- mask(kenya_stack, kenya_county_sp)
plot(kenya_stack)


# Problem 2 ---------------------------------------------------------------

# Overlay
light_area <- overlay(kenya_stack, fun = function(x,y) {x*y})

# Stack
kenya_stack <- stack(kenya_stack, light_area)


# Problem 3 ---------------------------------------------------------------

# Extract
kenya_ext <- raster::extract(kenya_stack, kenya_county_sp, fun="sum", na.rm = T, df = T)

# Names
names(kenya_ext) <- c("ID", "lights", "land_area", "lt_area")

# Join with kenya_county
kenya_county <- kenya_county %>% 
  mutate(ID = 1:nrow(.)) %>% 
  left_join(kenya_ext, by = "ID") %>% 
  mutate(avg_light = lt_area/land_area)


# Problem 4 ---------------------------------------------------------------

plot(kenya_county["avg_light"])

# log + 0.01
kenya_county <- kenya_county %>% 
  mutate(ln_avg_light = log(avg_light + 0.01))

# Plot Again
plot(kenya_county["ln_avg_light"])










#






# End Code
