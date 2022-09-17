# Homework 1, Exercise: Projections
# Created: 9/13/22
# Last Edited: 9/13/22
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

global.libraries <- c("tidyverse", "sf", "haven", "raster", "gstat", "viridis", "tmap")

results <- sapply(as.list(global.libraries), pkgTest)

# library(tidyverse)
# library(sf)
# library(haven)
# library(raster)
# library(gstat)
# library(viridis)
# library(tmap)

dir <- "/Users/atay508/Documents/George Mason/2022-23 Classes/Spatial/Spatial-Class-Materials-Fall-2022/Coding Exercise 1"

setwd(dir)
getwd()


# Problem 1 ---------------------------------------------------------------

# Dar Es Salaam: lat = -6.816111, long = 39.280278
# Moshi: lat = -3.334883, long = 37.340381

salaam_moshi <- tibble(city = c("Dar es Salaam", "Moshi"),
                       long = c(39.280278, 37.340381),
                       lat = c(-6.816111, -3.334883))
salaam_moshi

salaam_moshi_sf <- st_as_sf(salaam_moshi,
                            coords = c("long","lat"),
                            crs = 4326)


# Problem 2 ---------------------------------------------------------------

st_distance(salaam_moshi_sf)

# Distance using st_distance is 442,711.8 meters

dist(st_coordinates(salaam_moshi_sf), method = "euclidean")

# 3.985241

# Euclidean distance is the distance between each city in a weird long-lat unit,
# which doesn't really work given that the distance between long and lat lines
# changes depending on where they're located on the earth. This warped nature of
# straight lines on a sphere throws off euclidean distance calculations, as
# a right triangle (used for calculating euclidean distance) can contain more
# than 180 degrees


# Problem 3 ---------------------------------------------------------------

# Mollweide Projection

salaam_moshi_moll <- salaam_moshi_sf %>% 
  st_transform("+proj=moll")

st_distance(salaam_moshi_moll)

# Distance using st_distance is 466,378.5 meters

dist(st_coordinates(salaam_moshi_moll), method = "euclidean")

# Distance using dist() is the same for the Mollweide projection, 466,378.5 meters

# Azimuthal Projection

salaam_moshi_aeqd <- salaam_moshi_sf %>% 
  st_transform("+proj=aeqd")

st_distance(salaam_moshi_aeqd)

# Distance using st_distance is 464,736.3 meters

dist(st_coordinates(salaam_moshi_aeqd), method = "euclidean")

# Distance using dist() is the same for the Azimuthal projection, 464,736.3 meters

# These projections differed from problem 2 in that both distance calculations
# produced the same results. This was largely expected, as both are flattened
# projections of the spherical earth and distances are calculated without standard
# longitude and latitude. The differences in distance between the two projections
# are produced by differences in the projection style. The Azimuthal distance
# is the most accurate, as its primary goal is the preservation of distance.



#

# End Code
