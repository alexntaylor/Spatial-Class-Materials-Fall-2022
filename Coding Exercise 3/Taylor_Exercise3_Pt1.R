# Homework 3, Part 1
# Created: 9/22/22
# Last Edited: 9/22/22
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
p_load(raster, rgdal, tidyverse, sf, haven, gstat, tmap, units)

dir <- "/Users/atay508/Documents/George Mason/2022-23 Classes/Spatial/Spatial-Class-Materials-Fall-2022/Coding Exercise 3"

setwd(dir)
getwd()


# Problem 1 ---------------------------------------------------------------

# Load Data
kenya_pop <- raster("./data/pop_2000.tif")

# Rows, Columns, Cells
nrow(kenya_pop) # 1150
ncol(kenya_pop) # 808
ncell(kenya_pop) # 929200
raster::crs(kenya_pop) # Mollweide


# Problem 2 ---------------------------------------------------------------

cellStats(kenya_pop, "sum") # 40,996,516


# Problem 3 ---------------------------------------------------------------

crop <- extent(3586663, 3826663, -271700, 24700)

# Crop 
nairobi <- kenya_pop[crop, drop=F]

# Population in Nairobi
cellStats(nairobi, "sum") # 11,773,840 people

# Seems high... Google says Nairobi population is 4,397,073 (2019)


# Problem 4 ---------------------------------------------------------------

lights <- raster("./data/F142000.tif")
crs(pop_light)
crs(kenya_pop)

# Resample since they are in different CRSs
pop_light <- projectRaster(kenya_pop, lights, crs = "+proj=moll")

# Total Pop
cellStats(pop_light, "sum") # 47,052,951

# Larger than total population from 2


# Problem 5 ---------------------------------------------------------------

pop_agg <- aggregate(pop_light, fact=2, fun=sum)

# Total Pop
cellStats(pop_agg, "sum") # 47,052,951

# Same as total population in 4









#








# End Code
