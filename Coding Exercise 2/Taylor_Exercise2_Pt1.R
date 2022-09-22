# Homework 2, Part 1
# Created: 9/20/22
# Last Edited: 9/21/22
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

global.libraries <- c("tidyverse", "sf", "haven", "raster", "gstat", "viridis", "tmap", "units", "pacman")

results <- sapply(as.list(global.libraries), pkgTest)

library(pacman)
p_load(tidyverse, sf, haven, raster, gstat, virdis, tmap, units)

dir <- "/Users/atay508/Documents/George Mason/2022-23 Classes/Spatial/Spatial-Class-Materials-Fall-2022/Coding Exercise 2"

setwd(dir)
getwd()


# Open African Boundaries -------------------------------------------------

africa_sf <- st_read("./data/africa_scale.shp")


# Problem 1 ---------------------------------------------------------------

# Compute centroid of each country, compute point on the surface of each polygon. Plot both and compare locations.
plot(africa_sf[1], col=NA, main="Centroids vs. Surface Points", reset=F)
plot(st_centroid(africa_sf), col=2, pch=20, add=T)
plot(st_point_on_surface(africa_sf), col=3, pch=20, add=T)


# Problem 2 ---------------------------------------------------------------

# Do in 4 CRSs: Geo, Moll, Lambert Azimuthal EA, Azimuthal ED

# 2.1 Buffer geographic centroid by 1 degree or 111 km
# 2.2 Calculate areas of buffers based on geographic coordinates

# Geographic
africa_geo_buff <- st_centroid(africa_sf) %>% 
  st_buffer(dist=111000) %>%
  mutate(buff_area = st_area(.)) %>%
  mutate(buff_area = set_units(buff_area, km^2))
plot(africa_geo_buff[9], col=NA, reset=F, main="")
plot(st_centroid(africa_sf), col=2, add=T)

# Mollweide
africa_moll_buff <- st_centroid(africa_sf) %>% 
  st_transform("+proj=moll") %>% 
  st_buffer(dist=111000) %>%
  st_transform(4326) %>% 
  mutate(buff_area = st_area(.)) %>%
  mutate(buff_area = set_units(buff_area, km^2))
dev.off()
plot(africa_moll_buff[9], col=NA, reset=F, main="")
plot(st_centroid(africa_sf), col=2, add=T)

# Lambert Azimuthal
africa_lamaz_buff <- st_centroid(africa_sf) %>% 
  st_transform("+proj=laea") %>% 
  st_buffer(dist=111000) %>%
  st_transform(4326) %>% 
  mutate(buff_area = st_area(.)) %>%
  mutate(buff_area = set_units(buff_area, km^2))
plot(africa_lamaz_buff[9], col=NA, reset=F, main="")
plot(st_centroid(africa_sf), col=2, add=T)

# Azumithal
africa_az_buff <- st_centroid(africa_sf) %>% 
  st_transform("+proj=aeqd") %>% 
  st_buffer(dist=111000) %>%
  st_transform(4326) %>% 
  mutate(buff_area = st_area(.)) %>%
  mutate(buff_area = set_units(buff_area, km^2))
plot(africa_az_buff[9], col=NA, reset=F, main="")
plot(st_centroid(africa_sf), col=2, add=T)


# Problem 3 ---------------------------------------------------------------

# Compare Area Summary Stats
summary(africa_geo_buff$buff_area)
summary(africa_moll_buff$buff_area)
summary(africa_lamaz_buff$buff_area)
summary(africa_az_buff$buff_area)

# These results were somewhat expected. In particular, the uniformity of buffer areas across the board in the Mollweide
# projection was expected, as it's an equal area projection. However, the relatively small variation in areas in the
# Lambert Azimuthal projection was unexpected since it's also supposed to be an equal area projection.




#






# End Code
